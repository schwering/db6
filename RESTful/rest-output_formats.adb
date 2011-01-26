-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;

with DB.Types.Keys;

with REST.Log;

package body REST.Output_Formats is

   task body Populator_Type
   is
      use type Maps.Cursors.Cursor_Ref_Type;
      Writer      : Writer_Ref_Type := null;
      Max_Objects : Positive;
      Cancelled   : Boolean := False;
   begin
      select
         accept Initialize
           (Writer_Ref : in Writer_Ref_Type;
            Max_Objs   : in Natural)
         do
            Writer      := Writer_Ref;
            Max_Objects := Max_Objs;
         end Initialize;
      or
         accept Stop do
            Cancelled := True;
         end Stop;
      end select;

      if Cancelled then
         goto Ending;
      end if;

      Writer.Start_Anonymous_Object;

      for I in 1 .. Max_Objects loop
         select
            accept Stop do
               Log.Info ("Cancelling thread");
               Cancelled := True;
               requeue Stop; -- for final Stop
            end Stop;
         else
            null;
         end select;
         exit when Cancelled;
         exit when Queues.Is_Final (Writer.Queue);
         exit when Writer.Cursor = null; -- Cursor might be null from beginning

         declare
            Object_Initialized : Boolean := False;

            procedure Serialize_Object
              (Key   : in DB.Types.Keys.Key_Type;
               Value : in DB.Types.Values.Value_Type) is
            begin
               if not Object_Initialized then
                  Object_Initialized := True;
                  Writer.Start_Object (DB.Maps.Row_To_String (Key.Row));
               end if;
               Writer.Put_Value (DB.Maps.Column_To_String (Key.Column), Value);
            end Serialize_Object;

            EOF : Boolean;
         begin
            Maps.Cursors.Next (Writer.Cursor, Serialize_Object'Access, EOF);
            if Object_Initialized then
               Writer.End_Object;
            end if;
            exit when EOF;
         end;
      end loop;

      Writer.End_Object;
      Queues.Mark_Final (Writer.Queue);

      accept Stop;
      <<Ending>> null;

   exception
      when E : others =>
         Log.Error (E);
         Queues.Mark_Final (Writer.Queue);
   end Populator_Type;


   procedure Initialize_Writer
     (Writer            : in Writer_Ref_Type;
      Map               : in REST.Maps.Map_Ref_Type;
      URL_Path          : in Ada.Strings.Unbounded.Unbounded_String;
      Offset            : in Natural;
      Lower_Bound       : in DB.Maps.Bound_Type;
      Upper_Bound       : in DB.Maps.Bound_Type;
      Has_Column_Regexp : in Boolean;
      Column_Regexp     : in String;
      Max_Objects       : in Natural) is
   begin
      if Writer.Initialized then
         raise Stream_Error;
      end if;
      Writer.Initialized := True;
      Writer.Self := Writer;
      Writer.Cursor := Maps.Cursors.New_Cursor
         (Map, URL_Path, Offset, Lower_Bound, Upper_Bound,
          Has_Column_Regexp, Column_Regexp);
      Writer.Populator.Initialize (Writer, Max_Objects);
   end Initialize_Writer;


   procedure Read
     (Resource : in out Writer_Type;
      Buffer   :    out AS.Stream_Element_Array;
      Last     :    out AS.Stream_Element_Offset)
   is
      pragma Assert (Buffer'Component_Size =
                     Queues.Item_Array_Type'Component_Size); 
      use type AS.Stream_Element_Offset;
      Q_Buf : Queues.Item_Array_Type
        (Natural (Buffer'First) .. Natural (Buffer'Last));
      for Q_Buf'Address use Buffer'Address;
      Q_From : Positive := Q_Buf'First;
      Q_Last : Natural;
   begin
      pragma Assert (Buffer'Size = Q_Buf'Size);
      if Buffer'Length = 0 then
         Last := Buffer'First - 1;
         return;
      end if;

      loop
         Queues.Dequeue (Resource.Queue, Q_Buf (Q_From .. Q_Buf'Last), Q_Last);
         exit when Q_Last < Q_From;
         Q_From := Q_Last + 1;
      end loop;

      Last := AS.Stream_Element_Offset (Q_Last);
   end Read;


   procedure Close (Resource : in out Writer_Type)
   is
      use type Maps.Cursors.Cursor_Ref_Type;
   begin
      --Log.Info ("Closing stream (queue final = "&
        --Queues.Is_Final (Resource.Queue)'Img &")");
      -- How should the server be stopped if he is trying to Enqueue something
      -- and the queue is full? We would either have to abort or have a timeout
      -- in the enqueue statement. The latter sucks because the vast majority
      -- of enqueue operations succeeds. We already have enough concurrency
      -- overhead.
      Resource.Populator.Stop;
      Queues.Mark_Final (Resource.Queue);
      if Resource.Cursor /= null then
         Maps.Cursors.Release (Resource.Cursor);
      end if;
   exception
      when E : others =>
         Log.Error (E);
         raise;
   end Close;


   function End_Of_File (Resource : Writer_Type) return Boolean is
   begin
      return Queues.Is_Final (Resource.Queue);
   end End_Of_File;

end REST.Output_Formats;

