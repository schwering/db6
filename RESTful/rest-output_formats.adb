-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

with DB.Types.Keys;

with REST.Log;

package body REST.Output_Formats is

   task body Populator_Type
   is
      Stream           : Stream_Ref_Type;
      Max_Objects      : Positive;
      N_Objects        : Natural := 0;
      Last_Key         : DB.Maps.Key_Type;
      Last_Initialized : Boolean := False;
      Cancelled        : Boolean := False;
   begin
      accept Initialize
        (Stream_Ref : in Stream_Ref_Type;
         Max_Objs   : in Natural)
      do
         Stream      := Stream_Ref;
         Max_Objects := Max_Objs;
      end Initialize;

      Stream.Start_Anonymous_Array;

      loop
         <<Next_Iteration>>
         Log.Info ("Next in cursor");
         declare
            use type DB.Maps.Keys.Rows.String_Type;
            use type DB.Maps.Keys.Columns.String_Type;
            use type DB.Maps.State_Type;
            Key   : DB.Maps.Key_Type;
            Value : DB.Maps.Value_Wrapper_Type;
            State : DB.Maps.State_Type;
         begin
            select
               accept Stop do
                  Log.Info ("Cancelling thread");
                  Cancelled := True;
                  requeue Stop;
               end Stop;
            else
               null;
            end select;
            exit when Cancelled;
            exit when N_Objects > Max_Objects;
            exit when Queues.Is_Final (Stream.Queue);

            Stream.Cursor.Next (Key, Value, State);
            exit when State /= DB.Maps.Success;

            if Last_Initialized and then
               Last_Key.Row = Key.Row and then
               Last_Key.Column = Key.Column
            then
               -- We only take the most up-to-date version of each key.
               goto Next_Iteration;
            end if;

            if not Last_Initialized then
               Stream.Start_Object (DB.Maps.Row_To_String (Key.Row));
            elsif Last_Key.Row /= Key.Row then
               Stream.End_Object;
               N_Objects := N_Objects + 1;
               Log.Info ("Completed object "& N_Objects'Img);
               Stream.Start_Object (DB.Maps.Row_To_String (Key.Row));
            end if;

            Stream.Put_Value
              (DB.Maps.Column_To_String (Key.Column), Value.Ref.all);

            Last_Key         := Key;
            Last_Initialized := True;
         end;
      end loop;

      if Last_Initialized then
         -- Finish last object.
         Stream.End_Object;
      end if;

      Stream.End_Array;
      Queues.Mark_Final (Stream.Queue);

      Log.Info ("Stopping thread");
      accept Stop;

   exception
      when E : others =>
         Log.Error (E);
         Queues.Mark_Final (Stream.Queue);
   end Populator_Type;


   procedure Initialize_Stream
     (Stream        : in Stream_Ref_Type;
      Cursor        : in DB.Maps.Cursor_Ref_Type;
      Free_On_Close : in Boolean;
      Max_Objects   : in Natural) is
   begin
      if Stream.Initialized then
         raise Stream_Error;
      end if;
      Stream.Initialized   := True;
      Stream.Cursor        := Cursor;
      Stream.Self          := Stream;
      Stream.Free_On_Close := Free_On_Close;
      Stream.Populator.Initialize (Stream, Max_Objects);
   end Initialize_Stream;


   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out AS.Stream_Element_Array;
      Last     :    out AS.Stream_Element_Offset)
   is
      pragma Assert (Buffer'Component_Size =
                     Queues.Item_Array_Type'Component_Size); 
      use type AS.Stream_Element_Offset;
      Q_Buf : Queues.Item_Array_Type
        (Natural (Buffer'First) .. Natural (Buffer'Last));
      for Q_Buf'Address use Buffer'Address;
      Q_Last : Natural;
   begin
      pragma Assert (Buffer'Size = Q_Buf'Size);
      if Buffer'Length = 0 then
         Last := Buffer'First - 1;
         return;
      end if;

      Queues.Dequeue (Resource.Queue, Q_Buf, Q_Last);
      Last := AS.Stream_Element_Offset (Q_Last);
      Log.Info ("Read "& Last'Img);
   end Read;


   procedure Close (Resource : in out Stream_Type) is
   begin
      Log.Info ("Closing stream (queue final = "&
        Queues.Is_Final (Resource.Queue)'Img &")");
      -- How should the server be stopped if he is trying to Enqueue something
      -- and the queue is full? We would either have to abort or have a timeout
      -- in the enqueue statement. The latter sucks because the vast majority
      -- of enqueue operations succeeds. We already have enough concurrency
      -- overhead.
      Resource.Populator.Stop;
      Queues.Mark_Final (Resource.Queue);
      if Resource.Free_On_Close then
         declare
            --procedure Free is new Ada.Unchecked_Deallocation
              --(Stream_Type'Class, Stream_Ref_Type);
            procedure Free is new Ada.Unchecked_Deallocation
              (DB.Maps.Cursor_Type'Class, DB.Maps.Cursor_Ref_Type);
         begin
            Free (Resource.Cursor);
            --Free (Resource.Self);
         end;
      end if;
      declare
      begin
         raise Constraint_Error;
      exception
         when E : others =>
            Log.Error (E);
      end;
   end Close;


   function End_Of_File (Resource : Stream_Type) return Boolean is
   begin
      return Queues.Is_Final (Resource.Queue);
   end End_Of_File;

end REST.Output_Formats;

