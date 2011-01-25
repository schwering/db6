-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

with DB.Utils.Regexps.Cache;

with REST.Log;

package body REST.Maps.Cursors is

   procedure Free is new Ada.Unchecked_Deallocation
     (Cursor_Type, Cursor_Ref_Type);


   task body Killer_Task_Type
   is
      Self     : Cursor_Ref_Type;
      Continue : Boolean := True;
      Free     : Boolean := False;
   begin
      accept Start (Cursor : in Cursor_Ref_Type) do
         Self := Cursor;
      end Start;
      while Continue loop
         select
               accept Stop do
                  Continue := True;
               end Stop;
            or delay 60.0;
                  Continue := False;
                  Free     := True;
         end select;
      end loop;
      if Free then
         Trash.Throw_Away (Self);
      end if;
   end Killer_Task_Type;


   protected body Cursors is

      procedure Put (Cursor : in not null Cursor_Ref_Type)
      is
         pragma Precondition (Cursor.Released);
         use type Maps.Cursor;
         C : constant Maps.Cursor := Maps.Find (Map, Cursor.URL_Path);
      begin
         if C /= Maps.No_Element then
            declare
               procedure Append_Cursor
                 (Key   : in     Unbounded_String;
                  Value : in out Vectors.Vector)
               is
                  pragma Unreferenced (Key);
               begin
                  Vectors.Append (Value, Cursor);
               end Append_Cursor;
            begin
               Maps.Update_Element (Map, C, Append_Cursor'Access);
            end;
         else
            declare
               V : constant Vectors.Vector := Vectors.To_Vector (Cursor, 1);
            begin
               Maps.Insert (Map, Cursor.URL_Path, V);
            end;
         end if;
      end Put;


      procedure Get
        (URL_Path : in  Unbounded_String;
         Offset   : in  Natural;
         Cursor   : out Cursor_Ref_Type)
      is
         procedure Extract_Cursor
           (Key   : in     Unbounded_String;
            Value : in out Vectors.Vector)
         is
            pragma Unreferenced (Key);
            use type Vectors.Cursor;
            C : Vectors.Cursor := Vectors.First (Value);
         begin
            while C /= Vectors.No_Element loop
               exit when Vectors.Element (C).Offset = Offset;
               Vectors.Next (C);
            end loop;
            if C /= Vectors.No_Element then
               Cursor := Vectors.Element (C);
               Vectors.Delete (Value, C);
            else
               Cursor := null;
            end if;
         end Extract_Cursor;

         use type Maps.Cursor;
         C : constant Maps.Cursor := Maps.Find (Map, URL_Path);
      begin
         if C /= Maps.No_Element then
            Maps.Update_Element (Map, C, Extract_Cursor'Access);
         else
            Cursor := null;
         end if;
      end Get;


      procedure Kill (Cursor : in out Cursor_Ref_Type; Success : out Boolean) is
      begin
         Get (Cursor.URL_Path, Cursor.Offset, Cursor);
         if Cursor /= null then
            Finalize (Cursor);
            Success := True;
         else
            Success := False;
         end if;
      end Kill;


      procedure Kill_All
      is
         procedure Visit (C : Maps.Cursor)
         is
            procedure Finalize (C : Vectors.Cursor)
            is
               Ptr : Cursor_Ref_Type := Vectors.Element (C);
            begin
               Free (Ptr);
            end Finalize;
         begin
            Vectors.Iterate (Maps.Element (C), Finalize'Access);
         end Visit;
      begin
         Maps.Iterate (Map, Visit'Access);
      end Kill_All;

   end Cursors;


   protected body Trash is

      function Is_Full return Boolean
      is
         Full : Boolean := False;
      begin
         for I in Cursors'Range loop
            Full := Full and Cursors (I) /= null;
         end loop;
         return Full;
      end Is_Full;


      entry Throw_Away (Cursor : in not null Cursor_Ref_Type)
         when not Is_Full is
      begin
         for I in Cursors'Range loop
            if Cursors (I) = null then
               Cursors (I) := Cursor;
               exit;
            end if;
         end loop;
      end Throw_Away;


      entry Empty when Is_Full is
      begin
         for I in 1 .. Cursors'Last loop
            if Cursors (I) /= null and then Cursors (I).Killer'Terminated then
               Free (Cursors (I));
            end if;
         end loop;
      end Empty;

   end Trash;


   task body Cleaner_Task is
   begin
      loop
         Trash.Empty;
         Log.Info ("Emptied trash");
      end loop;
   end Cleaner_Task;


   function Bound
     (Row       : String;
      Inclusive : Boolean;
      Lower     : Boolean)
      return DB.Maps.Bound_Type
   is
      function Infinity return DB.Maps.Bound_Type is
      begin
         if Lower then
            return DB.Maps.Negative_Infinity_Bound;
         else
            return DB.Maps.Positive_Infinity_Bound;
         end if;
      end Infinity;

      function Comparison return DB.Maps.Comparison_Type is
      begin
         case Lower is
            when True =>
               case Inclusive is
                  when True  => return DB.Maps.Greater;
                  when False => return DB.Maps.Greater_Or_Equal;
               end case;
            when False =>
               case Inclusive is
                  when True  => return DB.Maps.Less;
                  when False => return DB.Maps.Less_Or_Equal;
               end case;
         end case;
      end Comparison;
   begin
      if Row = Infinity_Row then
         return Infinity;
      else
         -- How do we determine Max? Have a look at this truth table:
         -- Lower Inclusive => Max
         -- True  True      => False
         -- True  False     => True
         -- False True      => True
         -- False False     => False
         return DB.Maps.New_Bound
           (Comparison, Make_Key (Row, Max => Lower /= Inclusive));
      end if;
   end Bound;


   function New_Cursor
     (Map           : not null DB.Maps.Map_Ref_Type;
      URL_Path      : Unbounded_String;
      Offset        : Natural;
      Lower         : DB.Maps.Bound_Type;
      Upper         : DB.Maps.Bound_Type;
      Column_Regexp : String)
      return Cursor_Ref_Type
   is
      Cursor : Cursor_Ref_Type;
   begin
      Cursors.Get (URL_Path, Offset, Cursor);
      if Cursor /= null then
         pragma Assert (not Cursor.Released);
         Cursor.Released := False;
         return Cursor;
      else
         Cursor := new Cursor_Type'
           (URL_Path      => URL_Path,
            Cursor        => Map.New_Cursor_Ref
                              (Thread_Safe   => False,
                               Lower_Bound   => Lower,
                               Upper_Bound   => Upper,
                               Column_Regexp => Column_Regexp),
            Offset        => 0,
            Column_Regexp => DB.Utils.Regexps.Cache.Compile (Column_Regexp),
            Has_Last      => False,
            Last_Key      => DB.Types.Keys.Null_Key,
            Last_Value    => DB.Types.Values.Nothing_Value,
            Released      => False,
            Killer        => <>);
         Cursor.Killer.Start (Cursor);
         return Cursor;
      end if;
   end New_Cursor;


   procedure Release_Cursor (Cursor : in not null Cursor_Ref_Type) is
   begin
      Cursor.Released := True;
      Cursors.Put (Cursor);
   end Release_Cursor;


   procedure Next
     (Cursor  : in out Cursor_Ref_Type;
      Key     :    out DB.Types.Keys.Key_Type;
      Value   :    out DB.Types.Values.Value_Type;
      Success :    out Boolean) is
   begin
      if Cursor.Has_Last then
         Cursor.Has_Last := False;
         Cursor.Offset   := Cursor.Offset + 1;
         Key             := Cursor.Last_Key;
         Value           := Cursor.Last_Value;
         Success         := True;
      else
         declare
            use type DB.Maps.State_Type;
            State : DB.Maps.State_Type;
         begin
            Cursor.Cursor.Next (Key, Value, State);
            Success := State = DB.Maps.Success;
            if Success then
               Cursor.Offset := Cursor.Offset + 1;
            else
               Finalize (Cursor);
            end if;
         end;
      end if;
   end Next;


   procedure Push_Back
     (Cursor : in not null Cursor_Ref_Type;
      Key    : in          DB.Types.Keys.Key_Type;
      Value  : in          DB.Types.Values.Value_Type)
   is
      pragma Precondition (not Cursor.Has_Last);
   begin
      Cursor.Has_Last   := True;
      Cursor.Last_Key   := Key;
      Cursor.Last_Value := Value;
      Cursor.Offset     := Cursor.Offset - 1;
   end Push_Back;


   procedure Finalize (Cursor : in out Cursor_Ref_Type)
   is
      pragma Precondition (Cursor.Released);
      use type DB.Maps.Cursor_Ref_Type;
   begin
      if Cursor.Cursor /= null then
         Cursor.Cursor.Finalize;
      end if;
      Free (Cursor);
   end Finalize;

end REST.Maps.Cursors;

