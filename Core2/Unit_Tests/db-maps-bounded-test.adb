-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Strings.Bounded;

with AUnit.Assertions; use AUnit.Assertions;

with DB.Maps.Tag_Map;
with DB.Maps.Test_Utils;
with DB.Maps.Values.Integers;
with DB.Maps.Values.Strings;
with DB.Types.Keys; use DB.Types.Keys;

package body DB.Maps.Bounded.Test is

   File_Name : constant String := "unit_test_bounded_map";
   Loop_Count : constant := 100;

   function New_Key (Row, Col : String; Time : Natural) return Key_Type is
   begin
      return Key_Type'(Rows.New_String (Rows.Indefinite_Buffer_Type (Row)),
                       Columns.New_String (Rows.Indefinite_Buffer_Type (Col)),
                       Times.Number_Type (Time));
   end New_Key;


   function New_Key (I : Integer) return Key_Type
   is
      package Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (256);
      use Strings;

      type Pool_Type is array (Natural range <>) of Bounded_String;

     Pool : constant Pool_Type :=
       (To_Bounded_String ("Halleluja"),
        To_Bounded_String ("BMW"),
        To_Bounded_String ("Der neue X3"),
        To_Bounded_String ("Sportwagen"),
        To_Bounded_String ("Automotive Software"),
        To_Bounded_String ("Artificial Intelligence: A Modern Approach"),
        To_Bounded_String ("Stuart Russel"),
        To_Bounded_String ("Peter Norvig"),
        To_Bounded_String ("Hannelore Kraft"),
        To_Bounded_String ("Edmund Stoiber"),
        To_Bounded_String ("Oberst Wilhelm Klink"),
        To_Bounded_String ("Corporal Newkirk"),
        To_Bounded_String ("Feldwebel Georg Schultz"),
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


   procedure Test_Create (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      Map : Map_Type := New_Map;
   begin
      Create (Map, File_Name);
      Inserts (Map);
      Anti_Inserts (Map);
      Searches (Map);
      Finalize (Map);
   end;


   procedure Test_Open (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      Map : Map_Type := New_Map;
   begin
      Open (Map, File_Name);
      Appends (Map);
      Searches (Map);
      Deletes (Map, Anti_Search => False);
      Searches (Map);
      Deletes (Map, Anti_Search => True);
      Finalize (Map);
      Test_Utils.Unlink (File_Name);
   exception
      when others =>
         Test_Utils.Unlink (File_Name);
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
         K : Key_Type;
         V : Values.Integers.Value_Type;
         S : State_Type;
         N : Natural := 0;
      begin
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            C.Pause;
            N := N + 1;
         end loop;
         Assert (N = Loop_Count, "Met "& N'Img &" items where "&
                                 Loop_Count'Img &" were expected");
         C.Delete (K, V, S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
      end;

      procedure Single_Cursor_Delete
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Key_Type;
         V : Values.Integers.Value_Type;
         S : State_Type;
         N : Natural := 0;
      begin
         C.Delete (K, V, S);
         Assert (S = Failure, "Cursor had no hit no value but Delete didn't "&
                              "fail");
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            C.Delete (K, V, S);
            Assert (S = Success, "Deletion was not successful");
            N := N + 1;
         end loop;
         C.Delete (K, V, S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
         Assert (N = Loop_Count, "Met "& N'Img &" items where "&
                                 Loop_Count'Img &" were expected");
      end;

      procedure Single_Cursor_Pause_Delete
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Key_Type;
         V : Values.Integers.Value_Type;
         S : State_Type;
         N : Natural := 0;
      begin
         C.Delete (K, V, S);
         Assert (S = Failure, "Cursor had no hit no value but Delete didn't "&
                              "fail");
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            C.Pause;
            C.Delete (K, V, S);
            Assert (S = Success, "Deletion was not successful");
            N := N + 1;
         end loop;
         C.Delete (K, V, S);
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
         C.Delete (K, V, S);
         Assert (S = Failure, "Cursor had no hit no value but Delete didn't "&
                              "fail");
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            if N mod 2 = Modulo_Result then
               C.Delete (K, V, S);
               Assert (S = Success, "Deletion was not successful");
               M := M + 1;
            end if;
            N := N + 1;
         end loop;
         C.Delete (K, V, S);
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
         C.Delete (K, V, S);
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
         C.Delete (K, V, S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
         Assert (N = 0, "Met "& N'Img &" items where 0 were expected");
      end;

   begin
      Create (Map, File_Name);
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
      Test_Utils.Unlink (File_Name);
   exception
      when others =>
         Test_Utils.Unlink (File_Name);
         Assert (False, "3");
         raise;
   end;

end DB.Maps.Bounded.Test;

