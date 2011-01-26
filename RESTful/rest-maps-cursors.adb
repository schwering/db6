-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

with DB.Utils.Regexps.Cache;

with REST.Log;

package body REST.Maps.Cursors is

   ----------
   -- Cursor cache.

   protected body Cursors is

      procedure Put (Cursor : in not null Cursor_Ref_Type)
      is
         pragma Precondition (not Cursor.Released);
      begin
         Cursor.Released  := True;
         Cursor.Last_Used := Ada.Calendar.Clock;
         Multisets.Insert (Map, Cursor);
      end Put;


      procedure Get
        (URL_Path : in  Unbounded_String;
         Offset   : in  Natural;
         Cursor   : out Cursor_Ref_Type)
      is
         procedure Free is new Ada.Unchecked_Deallocation
           (Cursor_Type, Cursor_Ref_Type);
         use type Multisets.Cursor;
         Needle : Cursor_Ref_Type;
         C      : Multisets.Cursor;
      begin
         Needle := new Cursor_Type'(Search_Only => True,
                                    URL_Path    => URL_Path,
                                    Offset      => Offset,
                                    others      => <>);
         C := Multisets.Find (Map, Needle);
         Free (Needle);
         if C /= Multisets.No_Element then
            Cursor := Multisets.Element (C);
            Cursor.Released := False;
            Multisets.Delete (Map, C);
         else
            Cursor := null;
         end if;
      exception
         when others =>
            Free (Needle);
            raise;
      end Get;


      procedure Clean
      is
         package Vectors is new Ada.Containers.Vectors
           (Positive, Cursor_Ref_Type);

         function Too_Old (Cursor : not null Cursor_Ref_Type) return Boolean is
            use type Ada.Calendar.Time;
         begin
            return Ada.Calendar.Clock - Cursor.Last_Used > Timeout;
         end Too_Old;

         Delete : Vectors.Vector;
      begin
         declare
            use type Multisets.Cursor;
            Cursor : Cursor_Ref_Type;
            C      : Multisets.Cursor := Multisets.First (Map);
         begin
            while C /= Multisets.No_Element loop
               Cursor := Multisets.Element (C);
               pragma Assert (Cursor.Released);
               if Too_Old (Cursor) then
                  Vectors.Append (Delete, Cursor);
               end if;
              C := Multisets.Next (C);
            end loop;
         end;

         if Vectors.Is_Empty (Delete) then
            Log.Info ("Clean didn't find any old cursors");
         end if;

         declare
            use type Vectors.Cursor;
            Cursor : Cursor_Ref_Type;
            C      : Vectors.Cursor := Vectors.First (Delete);
         begin
            while C /= Vectors.No_Element loop
               Cursor := Vectors.Element (C);
               Log.Info ("Finalized cursor for URL "&
                         To_String (Cursor.URL_Path) &" with offset"&
                         Natural'Image (Cursor.Offset));
               Multisets.Delete (Map, Cursor);
               Finalize (Cursor);
               Vectors.Next (C);
            end loop;
         end;
      end Clean;

   end Cursors;


   ----------
   -- Cleaner task

   task Clean_Task;

   task body Clean_Task is
   begin
      loop
         delay Timeout;
         Cursors.Clean;
      end loop;
   end Clean_Task;


   ----------
   -- Cursor operations.

   function New_Cursor
     (Map               : not null DB.Maps.Map_Ref_Type;
      URL_Path          : Unbounded_String;
      Offset            : Natural;
      Lower             : DB.Maps.Bound_Type;
      Upper             : DB.Maps.Bound_Type;
      Has_Column_Regexp : Boolean;
      Column_Regexp     : String)
      return Cursor_Ref_Type
   is
      function Regexp return DB.Utils.Regexps.Regexp_Type is
      begin
         if Has_Column_Regexp then
            return DB.Utils.Regexps.Cache.Compile (Column_Regexp);
         else
            return DB.Utils.Regexps.Empty_Regexp;
         end if;
      end Regexp;

      Cursor : Cursor_Ref_Type;
   begin
      Cursors.Get (URL_Path, Offset, Cursor);
      if Cursor /= null then
         pragma Assert (not Cursor.Released);
         Log.Info ("Reusing cached cursor for URL "&
                   To_String (Cursor.URL_Path) &" with offset"&
                   Natural'Image (Cursor.Offset));
         return Cursor;
      else
         Cursor := new Cursor_Type'
           (Search_Only       => False,
            URL_Path          => URL_Path,
            Cursor            => Map.New_Cursor_Ref
                                 (Thread_Safe   => False,
                                  Lower_Bound   => Lower,
                                  Upper_Bound   => Upper,
                                  Column_Regexp => Column_Regexp),
            Offset            => 0,
            Has_Column_Regexp => Has_Column_Regexp,
            Column_Regexp     => Regexp,
            Reuse_Last        => False,
            Has_Last          => False,
            Last_Key          => DB.Types.Keys.Null_Key,
            Last_Value        => DB.Types.Values.Nothing_Value,
            Last_Used         => <>,
            Released          => False);
         Log.Info ("Created new cursor for URL "&
                   To_String (Cursor.URL_Path) &" with offset"&
                   Natural'Image (Cursor.Offset) &", still has to "&
                   "be moved to offset");
         for I in 1 .. Offset loop
            declare
               EOF : Boolean;
            begin
               Next (Cursor, null, EOF);
               exit when EOF;
            end;
         end loop;
         return Cursor;
      end if;
   end New_Cursor;


   procedure Release (Cursor : in not null Cursor_Ref_Type) is
   begin
      Cursors.Put (Cursor);
      Log.Info ("Released cursor for URL "&
                To_String (Cursor.URL_Path) &" with offset"&
                Natural'Image (Cursor.Offset));
   exception
      when E : others =>
         Log.Error (E);
         raise;
   end Release;


   procedure Next
     (Cursor  : in out Cursor_Ref_Type;
      Process : access procedure
                        (Key   : in DB.Types.Keys.Key_Type;
                         Value : in DB.Types.Values.Value_Type);
      EOF     :    out Boolean)
   is
      use type DB.Types.Keys.Rows.String_Type;
      use type DB.Types.Keys.Columns.String_Type;

      procedure Next_Pair
        (Key     : out DB.Types.Keys.Key_Type;
         Value   : out DB.Types.Values.Value_Type;
         EOF     : out Boolean) is
      begin
         if Cursor.Reuse_Last then
            Cursor.Reuse_Last := False;
            Cursor.Has_Last   := False;
            Key               := Cursor.Last_Key;
            Value             := Cursor.Last_Value;
            EOF               := False;
         else
            declare
               use type DB.Maps.State_Type;
               State : DB.Maps.State_Type;
            begin
               Cursor.Cursor.Next (Key, Value, State);
               EOF := State /= DB.Maps.Success;
            end;
         end if;
      end Next_Pair;

      Key   : DB.Types.Keys.Key_Type;
      Value : DB.Types.Values.Value_Type;
   begin
      loop
         <<Next_Iteration>>
         Next_Pair (Key, Value, EOF);
         exit when EOF;

         if Cursor.Has_Last and then
            Cursor.Last_Key.Row = Key.Row and then
            Cursor.Last_Key.Column = Key.Column
         then
            goto Next_Iteration;
            -- We only take the most up-to-date version of each column.
         end if;

         Cursor.Reuse_Last := Cursor.Has_Last and then
                              Key.Row /= Cursor.Last_Key.Row;
         Cursor.Has_Last   := True;
         Cursor.Last_Key   := Key;
         Cursor.Last_Value := Value;
         exit when Cursor.Reuse_Last;
         -- Break after one object.

         if Process /= null then
            Process (Key, Value);
         end if;
      end loop;

      if EOF then
         Finalize (Cursor);
      else
         Cursor.Offset := Cursor.Offset + 1;
      end if;
   end Next;


   procedure Increment_Offset
     (Cursor : in not null Cursor_Ref_Type) is
   begin
      Cursor.Offset := Cursor.Offset + 1;
   end Increment_Offset;


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
   end Push_Back;


   procedure Finalize (Cursor : in out Cursor_Ref_Type)
   is
      use type DB.Maps.Cursor_Ref_Type;
      procedure Free is new Ada.Unchecked_Deallocation
        (Cursor_Type, Cursor_Ref_Type);
   begin
      if Cursor.Cursor /= null then
         Cursor.Cursor.Finalize;
      end if;
      Free (Cursor);
   end Finalize;


   ----------
   -- Utility functions.

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


   function Less (A, B : Cursor_Ref_Type) return Boolean is
   begin
      return A.URL_Path < B.URL_Path or else
            (A.URL_Path = B.URL_Path and then A.Offset < B.Offset);
   end Less;


   function Equal (A, B : Cursor_Ref_Type) return Boolean is
   begin
      if A.Search_Only or B.Search_Only then
         return A = B or else
               (A.URL_Path = B.URL_Path and then A.Offset = B.Offset);
      else
         return A = B;
      end if;
   end Equal;

end REST.Maps.Cursors;

