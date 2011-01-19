-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Strings.Bounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;

with DB.Maps;
with DB.Maps.Tag_Map;
with DB.Maps.Test_Utils;
with DB.Maps.Values.Integers;
with DB.Maps.Values.Strings;
with DB.Types.Keys; use DB.Types.Keys;

package body DB.Maps.Covering.Test is

   package Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (256);

   Meta_File_Name : constant String := "unit_test_covering_map";
   Map_File_Names : constant array (Positive range <>) of Strings.Bounded_String :=
     (Strings.To_Bounded_String ("unit_test_bmw"),
      Strings.To_Bounded_String ("unit_test_ai"),
      Strings.To_Bounded_String ("unit_test_politics"),
      Strings.To_Bounded_String ("unit_test_kvh1"),
      Strings.To_Bounded_String ("unit_test_mist"));
   Loop_Count : constant := 1000;


   procedure Unlink_Files is
   begin
      for I in Map_File_Names'Range loop
         Test_Utils.Unlink (Strings.To_String (Map_File_Names (I)));
      end loop;
      Test_Utils.Unlink (Meta_File_Name);
   end Unlink_Files;


   function New_Key (Row, Col : String; Time : Natural) return Key_Type is
   begin
      return Key_Type'(Rows.New_String (Rows.Indefinite_Buffer_Type (Row)),
                       Columns.New_String (Rows.Indefinite_Buffer_Type (Col)),
                       Times.Number_Type (Time));
   end New_Key;


   function New_Key (I : Integer) return Key_Type
   is
      use Strings;
      type Pool_Type is array (Natural range <>) of Bounded_String;

     Pool : constant Pool_Type :=
       (To_Bounded_String ("Halleluja"),
        To_Bounded_String ("BMW Z4"),
        To_Bounded_String ("BMWs neuer 1er steht in der BILD"),
        To_Bounded_String ("AI book Artificial Intelligence: A Modern Approach"),
        To_Bounded_String ("AI Stuart Russel"),
        To_Bounded_String ("AI Peter Norvig"),
        To_Bounded_String ("Krafts Hannelore"),
        To_Bounded_String ("Stoibers Edmund"),
        To_Bounded_String ("Klink, Oberst Wilhelm"),
        To_Bounded_String ("Schultzi ist ein Genie"),
        To_Bounded_String ("Hogan"));

     Suffix : constant String := To_String (Pool ((I * I) mod Pool'Length));
   begin
      return New_Key (Integer'Image (I) & Suffix, Suffix, I * I);
   end New_Key;


   function New_Value (I : Integer) return Maps.Value_Type'Class is
   begin
      return Values.Integers.New_Value (I);
   end New_Value;


   --function New_Value (S : String) return Maps.Value_Type'Class is
   --begin
      --return Values.Strings.New_Value (S);
   --end New_Value;


   overriding
   procedure Set_Up (T : in out Test_Type) is
   begin
      Tag_Map.Utils.Store (T.State);
      Tag_Map.Clear;
      Tag_Map.Register (Values.Integers.Value_Type'Tag);
      Tag_Map.Register (Values.Strings.Value_Type'Tag);
      Tag_Map.Seal;
   end Set_Up;


   overriding
   procedure Tear_Down (T : in out Test_Type) is
   begin
      Tag_Map.Utils.Restore (T.State);
   end Tear_Down;


   procedure Test_Cover (T : in out Test_Type)
   is
      pragma Unreferenced (T);
   begin
      declare
         use Utils.Regular_Expressions;
         Guard : constant Slice_Array_Type :=
           ((Compile ("a*b"), null),
            (Compile ("b"), null),
            (Compile ("ab*"), null),
            (Compile ("c"), null),
            (Compile ("(aa)*(bb)*"), null),
            (Compile ("(bb)*(aa)*"), null));
         R : constant Regexp_Type := Compile ("a(a|b)b");
         Cov : constant Cover_Type := Cover (R, Guard);
         U : Regexp_Type := Empty_Regexp;
      begin
         for I in Cov'Range loop
            U := Union (U, Guard (Cov (I)).Guard);
         end loop;
         Assert (Is_Subset (R, U), "R is not a subset of U, even though "&
                                   "U should cover it.");
         Assert (Cov (1) = 1, "First element isn't Regexp 1");
         Assert (Cov (2) = 3, "First element isn't Regexp 3");
         Assert (Cov'Length = 2, "We have "& Cov'Length'Img &" elements "&
                                 "in the cover instead of 2.");
      end;
   end;


   procedure Inserts (Map : in out Maps.Map_Type'Class)
   is
      S : State_Type;
   begin
      for I in 1 .. Loop_Count loop
         Map.Insert (New_Key (I), New_Value (I), S);
         Assert (S = Success, "Insertion failed: "&
                              Types.Keys.Image (New_Key (I)) &"  /  "&
                              New_Value (I).Image);
         declare
            V : Values.Integers.Value_Type;
         begin
            Map.Search (New_Key (I), V, S);
            Assert (S = Success, Types.Keys.Image (New_Key (I)) &" is not in "&
                                 "the map");
         end;
      end loop;
   end;


   procedure Appends (Map : in out Maps.Map_Type'Class)
   is
      S : State_Type;
   begin
      for I in 1 .. Loop_Count loop
         Map.Append (New_Key (I), New_Value (I), S);
         Assert (S = Success, "Append failed: "&
                              Types.Keys.Image (New_Key (I)) &"  /  "&
                              New_Value (I).Image);
         declare
            V : Values.Integers.Value_Type;
         begin
            Map.Search (New_Key (I), V, S);
            Assert (S = Success, Types.Keys.Image (New_Key (I)) &" is not in "&
                                 "the map");
         end;
      end loop;
   end;


   procedure Anti_Inserts (Map : in out Maps.Map_Type'Class)
   is
      S : State_Type;
   begin
      for I in 1 .. Loop_Count loop
         Map.Insert (New_Key (I), New_Value (I), S);
         Assert (S = Failure, "Duplicate insertion successful: "&
                              Types.Keys.Image (New_Key (I)) &"  /  "&
                              New_Value (I).Image);
      end loop;
   end;


   procedure Searches (Map : in out Maps.Map_Type'Class)
   is
      V : Values.Integers.Value_Type;
      S : State_Type;
   begin
      for I in 1 .. Loop_Count loop
         Map.Search (New_Key (I), V, S);
         Assert (S = Success, "Search failed: "&
                              Types.Keys.Image (New_Key (I)));
      end loop;
   end;


   procedure Deletes (Map         : in out Maps.Map_Type'Class;
                      Anti_Search : in     Boolean)
   is
      V : Values.Integers.Value_Type;
      S : State_Type;
   begin
      for I in 1 .. Loop_Count loop
         Map.Delete (New_Key (I), V, S);
         Assert (S = Success, "Delete failed: "&
                              Types.Keys.Image (New_Key (I)));
         Assert (V.Equals(New_Value (I)),
                 "Unexpected value "& V.Image &" vs "& New_Value (I).Image);
         if Anti_Search then
            Map.Search (New_Key (I), V, S);
            Assert (S = Failure, Types.Keys.Image (New_Key (I)) &" is still in"&
                                 " the map with value "& New_Value (I).Image);
            Map.Delete (New_Key (I), V, S);
            Assert (S = Failure, Types.Keys.Image (New_Key (I)) &" could still"&
                                 " be deleted from the map with value "&
                                 New_Value (I).Image);
         end if;
      end loop;
   end;


   procedure Configure_Map (Map : in out Map_Type'Class)
   is
      use type DB.Maps.Implementation_Type;
      use Utils.Regular_Expressions;
   begin
      Map.Add_Slice ("BMW.*", BTree, Strings.To_String (Map_File_Names (1)));
      Map.Add_Slice ("AI.*", BTree, Strings.To_String (Map_File_Names (2)));
      Map.Add_Slice ("(Kraft.*)|(Stoiber.*)", BTree,
                     Strings.To_String (Map_File_Names (3)));
      Map.Add_Slice ("(Hogan.*)|(Klink.*)", BTree,
                     Strings.To_String (Map_File_Names (4)));
      Map.Add_Slice (".*", BTree, Strings.To_String (Map_File_Names (5)));
   end;


   procedure Test_Create (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      Map : Map_Type := New_Map;
   begin
      Configure_Map (Map);
      Create (Map, Meta_File_Name);
      Assert (Map.Slices'Length = 5, "We have "& Map.Slices'Length'Img &
                                     " slices instead of 5");
      Assert (Map.Cover'Length = 5, "Cover has size "& Map.Cover'Length'Img &
                                    " instead of 5");
      Inserts (Map);
      Anti_Inserts (Map);
      Searches (Map);
      Finalize (Map);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         Unlink_Files;
         raise;
   end;


   procedure Test_Open (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      Map : Map_Type := New_Map;
   begin
      --Configure_Map (Map);
      Open (Map, Meta_File_Name);
      Assert (Map.Slices'Length = 5, "We have "& Map.Slices'Length'Img &
                                     " slices instead of 5");
      Assert (Map.Cover'Length = 5, "Cover has size "& Map.Cover'Length'Img &
                                    " instead of 5");
      Appends (Map);
      Searches (Map);
      Deletes (Map, Anti_Search => False);
      Searches (Map);
      Deletes (Map, Anti_Search => True);
      Finalize (Map);
      Unlink_Files;
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         Unlink_Files;
         raise;
   end;


   procedure Test_Cursor (T : in out Test_Type)
   is
      pragma Unreferenced (T);

      Map : Map_Type := New_Map;

      procedure Single_Cursor_Pause
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         P : Key_Type;
         K : Key_Type;
         V : Values.Integers.Value_Type;
         S : State_Type;
         N : Natural := 0;
         pragma Warnings (Off, P);
      begin
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            Assert (N = 0 or else P <= K,
                    "Previous key "& Types.Keys.Image (P) &" is not "&
                    "less than or equal to "& Types.Keys.Image (K));
            C.Pause;
            if N = 0 or else P /= K then
               N := N + 1;
            end if;
            P := K;
         end loop;
         Assert (N = Loop_Count, "Met "& N'Img &" items where "&
                                 Loop_Count'Img &" were expected");
         C.Delete (S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
      end;

      procedure Single_Cursor_Delete
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         P : Key_Type;
         K : Key_Type;
         V : Values.Integers.Value_Type;
         S : State_Type;
         N : Natural := 0;
         pragma Warnings (Off, P);
      begin
         C.Delete (S);
         Assert (S = Failure, "Cursor had no hit no value but Delete didn't "&
                              "fail");
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            C.Delete (S);
            Assert (S = Success, "Deletion was not successful");
            if N = 0 or else P /= K then
               N := N + 1;
            end if;
            P := K;
         end loop;
         C.Delete (S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
         Assert (N = Loop_Count, "Met "& N'Img &" items where "&
                                 Loop_Count'Img &" were expected");
      end;

      procedure Single_Cursor_Pause_Delete
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Key_Type;
         P : Key_Type;
         V : Values.Integers.Value_Type;
         S : State_Type;
         N : Natural := 0;
         pragma Warnings (Off, P);
      begin
         C.Delete (S);
         Assert (S = Failure, "Cursor had no hit no value but Delete didn't "&
                              "fail");
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            C.Pause;
            C.Delete (S);
            Assert (S = Success, "Deletion was not successful");
            if N = 0 or else P /= K then
               N := N + 1;
            end if;
            P := K;
         end loop;
         C.Delete (S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
         Assert (N = Loop_Count, "Met "& N'Img &" items where "&
                                 Loop_Count'Img &" were expected");
      end;

      procedure Half_Cursor_Delete (Even : in Boolean)
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Key_Type;
         V : Values.Integers.Value_Type;
         S : State_Type;
         N : Natural := 0;
         M : Natural := 0;
         Modulo_Result : Natural;
      begin
         if Even then
            Modulo_Result := 0;
         else
            Modulo_Result := 1;
         end if;
         C.Delete (S);
         Assert (S = Failure, "Cursor had no hit no value but Delete didn't "&
                              "fail");
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            if N mod 2 = Modulo_Result then
               C.Delete (S);
               Assert (S = Success, "Deletion was not successful");
               M := M + 1;
            end if;
            N := N + 1;
         end loop;
         C.Delete (S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
         Assert (N = Loop_Count, "Met "& N'Img &" items where "&
                                 Loop_Count'Img &" were expected");
         Assert (M = Loop_Count/2, "Deleted "& N'Img &" items where "&
                                   Loop_Count'Img &" were expected");
      end;

      procedure Double_Cursor
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Key_Type;
         V : Values.Integers.Value_Type;
         S : State_Type;
         N : Natural := 0;
      begin
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            N := N + 1;
         end loop;
         Assert (N = 2 * Loop_Count, "Met "& N'Img &" items where "&
                                     Loop_Count'Img &" were expected");
         C.Delete (S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
      end;

      procedure Empty
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Key_Type;
         V : Values.Integers.Value_Type;
         S : State_Type;
         N : Natural := 0;
      begin
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            N := N + 1;
         end loop;
         C.Delete (S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
         Assert (N = 0, "Met "& N'Img &" items where 0 were expected");
      end;

   begin
      Configure_Map (Map);
      Create (Map, Meta_File_Name);
      Appends (Map);
      Single_Cursor_Pause;
      Single_Cursor_Delete;
      Empty;
      Appends (Map);
      Appends (Map);
      Double_Cursor;
      Half_Cursor_Delete (Even => True);
      Half_Cursor_Delete (Even => False);
      Single_Cursor_Pause_Delete;
      Empty;
      Finalize (Map);
      Unlink_Files;
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         Unlink_Files;
         raise;
   end;

end DB.Maps.Covering.Test;

