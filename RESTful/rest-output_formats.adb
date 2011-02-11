-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with DB.Types.Keys;

with REST.Log;

package body REST.Output_Formats is

   task body Populator_Type
   is
      use type Maps.Cursors.Cursor_Ref_Type;
      Writer      : Writer_Ref_Type := null;
      Max_Objects : Positive;
      Next_URL    : Unbounded_String;
      Cancelled   : Boolean := False;
   begin
      select
         accept Initialize
           (Writer      : in Writer_Ref_Type;
            Max_Objects : in Natural;
            Next_URL    : in Unbounded_String)
         do
            Populator_Type.Writer      := Writer;
            Populator_Type.Max_Objects := Max_Objects;
            Populator_Type.Next_URL    := Next_URL;
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
            use type DB.Types.Keys.Columns.String_Type;
            In_Object        : Boolean := False;
            In_Array         : Boolean := False;
            Have_Last_Column : Boolean := False;
            Last_Column      : DB.Types.Keys.Columns.String_Type;

            procedure Serialize_Object
              (Key : in DB.Types.Keys.Key_Type;
               Val : in DB.Types.Values.Value_Type) is
            begin
               -- XXX TODO this doesn't work, because the first object of the
               -- array would always be processed with Put_Value.
               if not In_Object then
                  In_Object := True;
                  Writer.Start_Object (DB.Types.Keys.Rows.Image (Key.Row));
               end if;
               if not Have_Last_Column or Last_Column /= Key.Column then
                  if In_Array then
                     Writer.End_Array;
                     In_Array := False;
                  end if;
                  Writer.Put_Value
                    (DB.Types.Keys.Columns.Image (Key.Column), Val);
               else
                  if not In_Array then
                     In_Array := True;
                     Writer.Start_Array
                       (DB.Types.Keys.Columns.Image (Key.Column));
                  end if;
                  Writer.Put_Anonymous_Value (Val);
               end if;
               Have_Last_Column := True;
               Last_Column      := Key.Column;
            end Serialize_Object;

            EOF : Boolean;
         begin
            Maps.Cursors.Next (Writer.Cursor, Serialize_Object'Access, EOF);
            if In_Array then
               Writer.End_Array;
            end if;
            if In_Object then
               Writer.End_Object;
            end if;
            exit when EOF;
         end;
      end loop;

      if Writer.Cursor = null or Length (Next_URL) = 0 then
         Writer.Put_Value (Next_URL_Key, DB.Types.Values.Nothing_Value);
      else
         Writer.Put_Value
           (Next_URL_Key, DB.Types.Values.New_Value (To_String (Next_URL)));
      end if;

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
      URL_Path          : in Unbounded_String;
      Offset            : in Natural;
      Lower_Bound       : in DB.Maps.Bound_Type;
      Upper_Bound       : in DB.Maps.Bound_Type;
      Has_Column_Regexp : in Boolean;
      Column_Regexp     : in String;
      Max_Objects       : in Natural;
      Next_URL          : in Unbounded_String) is
   begin
      if Writer.Initialized then
         raise Stream_Error;
      end if;
      Writer.Initialized := True;
      Writer.Self := Writer;
      Writer.Cursor := Maps.Cursors.New_Cursor
         (Map, URL_Path, Offset, Lower_Bound, Upper_Bound,
          Has_Column_Regexp, Column_Regexp);
      Writer.Populator.Initialize (Writer, Max_Objects, Next_URL);
   end Initialize_Writer;


   procedure Read
     (Resource : in out Writer_Type;
      Buffer   :    out AS.Stream_Element_Array;
      Last     :    out AS.Stream_Element_Offset)
   is
      pragma Assert (Buffer'Component_Size =
                     Queues.Item_Array_Type'Component_Size); 
      use type AS.Stream_Element_Offset;
      Q_Buffer : Queues.Item_Array_Type (1 .. Buffer'Length);
      Q_From   : Positive := Q_Buffer'First;
      Q_Last   : Natural;
   begin
      pragma Assert (Buffer'Size = Q_Buffer'Size);
      if Buffer'Length = 0 then
         Last := Buffer'First - 1;
         return;
      end if;

      loop
         Queues.Dequeue
           (Resource.Queue, Q_Buffer (Q_From .. Q_Buffer'Last), Q_Last);
         exit when Q_Last not in Q_From .. Q_Buffer'Last;
         Q_From := Q_Last + 1;
      end loop;

      Last := AS.Stream_Element_Offset
        (Buffer'First + AS.Stream_Element_Offset (Q_Last - Q_Buffer'First));
      for I in Buffer'First .. Last loop
         Buffer (I) := Q_Buffer (Q_Buffer'First + Natural (I - Buffer'First));
      end loop;
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

