-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with AUnit.Assertions; use AUnit.Assertions;

with AWS.Client;
with AWS.Response;
with AWS.Server;

with DB.Maps;
with DB.Maps.Values;
with DB.Maps.Values.Booleans;
with DB.Maps.Values.Long_Floats;
with DB.Maps.Values.Long_Integers;
with DB.Maps.Values.Nothings;
with DB.Maps.Values.Strings;

with REST.Method;
with REST.Maps.Test_Utils; use REST.Maps.Test_Utils;

package body REST.Get.Test is

   overriding
   procedure Set_Up (T : in out Test_Type) is
   begin
      null;
   end Set_Up;


   overriding
   procedure Tear_Down (T : in out Test_Type) is
   begin
      null;
   end Tear_Down;

   
   procedure Insert (Table : in String; KVs : in Key_Value_Array_Type)
   is
      Map : constant REST.Maps.Map_Ref_Type := REST.Maps.Map_By_Name (Table);
   begin
      for I in KVs'Range loop
         declare
            use DB.Maps.Values;
            use type DB.Maps.State_Type;
            Key   : constant DB.Maps.Key_Type := 
               DB.Maps.Strings_To_Key (TS (KVs (I).Row), TS (KVs (I).Column));
            Val   : constant String := TS (KVs (I).Value);
            State : DB.Maps.State_Type;
         begin
            case Val (Val'First) is
               when ''' =>
                  Map.Insert
                    (Key,
                     Strings.New_Value (Val (Val'First+1 ..  Val'Last-1)),
                     State);
               when '0' .. '9' =>
                  Map.Insert
                    (Key,
                     Long_Integers.New_Value
                       (Long_Integers.Integer_Type'Value (Val)),
                     State);
               when '-' =>
                  Map.Insert
                    (Key,
                     Long_Floats.New_Value
                       (Long_Floats.Float_Type'Value (Val)),
                     State);
               when 't' | 'f' =>
                  Map.Insert
                    (Key,
                     Booleans.New_Value
                       (Booleans.Integer_Type'Value (Val)),
                     State);
               when 'n' =>
                  Map.Insert (Key, Nothings.New_Value, State);
               when others =>
                  Assert (False, "Value '"& Val &"' invalid");
            end case;
            Assert (State = DB.Maps.Success,
                    "Insertion of "& TS (KVs (I).Row) &" / "&
                    TS (KVs (I).Column) &" / "&
                    TS (KVs (I).Value) &" failed");
         end;
      end loop;
   end Insert;

   KVs : constant Key_Value_Array_Type :=
     (KV ("person1", "vorname", "'Sergey'"),
      KV ("person1", "nachname", "'Brin'"),
      KV ("person1", "alter", "37"),
      KV ("person2", "vorname", "'Larry'"),
      KV ("person2", "nachname", "'Page'"),
      KV ("person2", "alter", "37"),
      KV ("person3", "vorname", "'Julietta'"),
      KV ("person3", "nachname", "'Youtubehausenspetergoogle'"),
      KV ("person3", "alter", "25"),
      KV ("person4", "vorname", "'Eva'"),
      KV ("person4", "nachname", "'Gruenwaldenshausen'"),
      KV ("person4", "alter", "26"),
      KV ("person5", "vorname", "'Rudi'"),
      KV ("person5", "nachname", "'Fichtenwald'"),
      KV ("person5", "alter", "24"),
      KV ("person6", "gehalt", "-1.234"),
      KV ("person6", "doof", "true"),
      KV ("person6", "klug", "false"),
      KV ("person6", "auto", "null"));


   function JSON_Contains (Data, Row, Col, Val : String) return Boolean is
      function Raw_Val return String is
      begin
         if Val'Length > 0 and then Val (Val'First) = ''' then
            return Val (Val'First + 1 .. Val'Last - 1);
         else
            return Val;
         end if;
      end Raw_Val;
      use Ada.Strings.Fixed;
      I, J : Natural;
   begin
      -- Find JSON object belonging to Row.
      I := Index (Data, Row);
      if I not in Data'Range then return False; end if;
      I := Index (Data (I .. Data'Last), "{");
      if I not in Data'Range then return False; end if;
      J := Index (Data (I .. Data'Last), "}");
      if J not in Data'Range then return False; end if;
      -- Find line belonging to Col.
      I := Index (Data (I .. J), Col);
      if I not in Data'Range then return False; end if;
      J := Index (Data (I .. J), (1 => ASCII.LF));
      if J not in Data'Range then return False; end if;
      -- Find the value.
      I := Index (Data (I .. J), Raw_Val);
      return I in Data'Range;
   end JSON_Contains;


   procedure Get
     (Table          : in String;
      Row_1, Row_2   : in String;
      Excl_1, Excl_2 : in Boolean;
      Count          : in Positive;
      Do_Insert      : in Boolean := True) is
   begin
      if Do_Insert then
         Insert (Table, KVs);
      end if;
      declare
         Response : constant AWS.Response.Data :=
            AWS.Client.Get
              (URL (Table, "*", Row_1, Row_2, Excl_1, Excl_2, Count));
         Data : constant String := AWS.Response.Message_Body (Response);
         Last_Row             : Unbounded_String;
         Last_Row_Initialized : Boolean := False;
         Row_Count            : Natural := 0;
      begin
         for I in KVs'Range loop
            if (Row_1 = Method.Infinity_Row or else
                (not Excl_1 and then Row_1 <= KVs (I).Row) or else
                (    Excl_1 and then Row_1 <  KVs (I).Row)) and then
               (Row_2 = Method.Infinity_Row or else
                (not Excl_2 and then KVs (I).Row <= Row_2) or else
                (    Excl_2 and then KVs (I).Row <  Row_2))
            then
               if not Last_Row_Initialized or else KVs (I).Row /= Last_Row then
                  Last_Row_Initialized := True;
                  Last_Row             := KVs (I).Row;
                  Row_Count            := Row_Count + 1;
                  exit when Row_Count > Count;
               end if;
               declare
                  Row : constant String := TS (KVs (I).Row);
                  Col : constant String := TS (KVs (I).Column);
                  Val : constant String := TS (KVs (I).Value);
               begin
                  Assert (Row_Count <= Count and
                          JSON_Contains (Data, Row, Col, Val),
                          "Body doesn't contain "& Row &" / "& Col &" / "&
                          Val &"("& Row_Count'Img &")"& ASCII.LF &
                          URL (Table, "*", Row_1, Row_2, Excl_1, Excl_2, Count)
                          & ASCII.LF & Data);
               end;
            end if;
         end loop;
      end;
   end Get;


   procedure Test_Excl_Excl (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Get
        (Table  => "test",
         Row_1  => "person2", Excl_1 => True,
         Row_2  => "person5", Excl_2 => True,
         Count  => 100);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end;


   procedure Test_Excl_Incl (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Get
        (Table  => "test",
         Row_1  => "person2", Excl_1 => True,
         Row_2  => "person5", Excl_2 => False,
         Count  => 100);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end;


   procedure Test_Incl_Excl (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Get
        (Table  => "test",
         Row_1  => "person2", Excl_1 => False,
         Row_2  => "person5", Excl_2 => True,
         Count  => 100);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end;


   procedure Test_Incl_Incl (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Get
        (Table  => "test",
         Row_1  => "person2", Excl_1 => False,
         Row_2  => "person5", Excl_2 => False,
         Count  => 100);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end;


   procedure Test_Empty_Excl_Excl (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Get
        (Table  => "test",
         Row_1  => "person3", Excl_1 => True,
         Row_2  => "person2", Excl_2 => True,
         Count  => 100);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end;


   procedure Test_Empty_Excl_Incl (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Get
        (Table  => "test",
         Row_1  => "person3", Excl_1 => True,
         Row_2  => "person2", Excl_2 => False,
         Count  => 100);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end;


   procedure Test_Empty_Incl_Excl (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Get
        (Table  => "test",
         Row_1  => "person3", Excl_1 => False,
         Row_2  => "person2", Excl_2 => True,
         Count  => 100);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end;


   procedure Test_Empty_Incl_Incl (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Get
        (Table  => "test",
         Row_1  => "person3", Excl_1 => False,
         Row_2  => "person2", Excl_2 => False,
         Count  => 100);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end;


   procedure Test_Infinity (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Get
        (Table     => "test",
         Row_1     => Method.Infinity_Row, Excl_1 => False,
         Row_2     => Method.Infinity_Row, Excl_2 => False,
         Count     => 100,
         Do_Insert => True);
      Get
        (Table     => "test",
         Row_1     => Method.Infinity_Row, Excl_1 => False,
         Row_2     => Method.Infinity_Row, Excl_2 => True,
         Count     => 100,
         Do_Insert => False);
      Get
        (Table     => "test",
         Row_1     => Method.Infinity_Row, Excl_1 => True,
         Row_2     => Method.Infinity_Row, Excl_2 => False,
         Count     => 100,
         Do_Insert => False);
      Get
        (Table     => "test",
         Row_1     => Method.Infinity_Row, Excl_1 => True,
         Row_2     => Method.Infinity_Row, Excl_2 => True,
         Count     => 100,
         Do_Insert => False);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end;


   procedure Test_Count (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Get
        (Table  => "test",
         Row_1  => "person1", Excl_1 => False,
         Row_2  => "person6", Excl_2 => False,
         Count  => 1); -- inserts the elements
      for I in 2 .. 10 loop
         Get
           (Table     => "test",
            Row_1     => "person1", Excl_1 => False,
            Row_2     => "person6", Excl_2 => False,
            Count     => I,
            Do_Insert => False);
         Get
           (Table     => "test",
            Row_1     => "person1", Excl_1 => True,
            Row_2     => "person6", Excl_2 => False,
            Count     => I,
            Do_Insert => False);
         Get
           (Table     => "test",
            Row_1     => "person1", Excl_1 => True,
            Row_2     => "person6", Excl_2 => False,
            Count     => I,
            Do_Insert => False);
         Get
           (Table     => "test",
            Row_1     => "person1", Excl_1 => True,
            Row_2     => "person6", Excl_2 => True,
            Count     => I,
            Do_Insert => False);
         Get
           (Table     => "test",
            Row_1     => Method.Infinity_Row, Excl_1 => False,
            Row_2     => Method.Infinity_Row, Excl_2 => False,
            Count     => I,
            Do_Insert => False);
      end loop;
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end;

end REST.Get.Test;
