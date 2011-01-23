-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Strings.Bounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;

with DB.Maps.Test_Utils;
with DB.Types.Keys;
with DB.Types.Values;

package body DB.Maps.Bounded.Test is
   use DB.Types;

   File_Name : constant String := "unit_test_bounded_map";
   Loop_Count : constant := 100;

   function New_Key (Row, Col : String; Time : Natural) return Keys.Key_Type is
   begin
      return (Keys.Rows.New_String (Keys.Rows.Indefinite_Buffer_Type (Row)),
              Keys.Columns.New_String (Keys.Rows.Indefinite_Buffer_Type (Col)),
              Keys.Times.Number_Type (Time));
   end New_Key;


   function New_Key (I : Integer) return Keys.Key_Type
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

     function To_String (I : Integer; Len : Integer) return String
     is
        T : constant String := I'Img;
        S : String (1 .. Integer'Max (Len, T'Length)) := (others => ' ');
     begin
        S (S'Last - T'Length + 1 .. S'Last) := T;
        for I in S'Range loop
           if S (I) = ' ' then
             S (I) := '0';
           end if;
        end loop;
        return S;
     end To_String;

     Int    : constant String := To_String (I, Loop_Count'Img'Length);
     Suffix : constant String := To_String (Pool ((I * I) mod Pool'Length));
   begin
      return New_Key (Int & Suffix, Suffix, I * I);
   end New_Key;


   function New_Value (I : Integer) return Values.Value_Type is
   begin
      return Values.New_Value (I);
   end New_Value;


   --function New_Value (S : String) return Values.Value_Type is
   --begin
      --return Values.New_Value (S);
   --end New_Value;


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


   procedure Inserts (Map : in out Maps.Map_Type'Class)
   is
      S : State_Type;
   begin
      for I in 1 .. Loop_Count loop
         Map.Insert (New_Key (I), New_Value (I), S);
         Assert (S = Success, "Insertion failed: "&
                              Keys.Image (New_Key (I)) &"  /  "&
                              Values.Image (New_Value (I)));
         declare
            V : Values.Value_Type;
         begin
            Map.Search (New_Key (I), V, S);
            Assert (S = Success, Keys.Image (New_Key (I)) &" is not in "&
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
                              Keys.Image (New_Key (I)) &"  /  "&
                              Values.Image (New_Value (I)));
         declare
            V : Values.Value_Type;
         begin
            Map.Search (New_Key (I), V, S);
            Assert (S = Success, Keys.Image (New_Key (I)) &" is not in "&
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
                              Keys.Image (New_Key (I)) &"  /  "&
                              Values.Image (New_Value (I)));
      end loop;
   end;


   procedure Searches (Map : in out Maps.Map_Type'Class)
   is
      V : Values.Value_Type;
      S : State_Type;
   begin
      for I in 1 .. Loop_Count loop
         Map.Search (New_Key (I), V, S);
         Assert (S = Success, "Search failed: "&
                              Keys.Image (New_Key (I)));
      end loop;
   end;


   procedure Deletes (Map         : in out Maps.Map_Type'Class;
                      Anti_Search : in     Boolean)
   is
      use type Values.Value_Type;
      V : Values.Value_Type;
      S : State_Type;
   begin
      for I in 1 .. Loop_Count loop
         Map.Delete (New_Key (I), V, S);
         Assert (S = Success, "Delete failed: "&
                              Keys.Image (New_Key (I)));
         Assert (V = New_Value (I),
                 "Unexpected value "& Values.Image (V) &" vs "&
                 Values.Image (New_Value (I)));
         if Anti_Search then
            Map.Search (New_Key (I), V, S);
            Assert (S = Failure, Keys.Image (New_Key (I)) &" is still in"&
                                 " the map with value "&
                                 Values.Image (New_Value (I)));
            Map.Delete (New_Key (I), V, S);
            Assert (S = Failure, Keys.Image (New_Key (I)) &" could still"&
                                 " be deleted from the map with value "&
                                 Values.Image (New_Value (I)));
         end if;
      end loop;
   end;


   procedure Delete_Range (Map   : in out Maps.Map_Type'Class;
                           First : in     Integer;
                           Last  : in     Integer)
   is
      V : Values.Value_Type;
      S : State_Type;
   begin
      for I in Integer'Max (First, 1) .. Integer'Min (Last, Loop_Count) loop
         Map.Search (New_Key (I), V, S);
         Assert (S = Success, "Item in range doesn't exist "&
                              Keys.Image (New_Key (I)));
      end loop;

      Map.Delete_Range (New_Key (First), New_Key (Last), S);
      Assert (S = Success, "Delete range failed "&
                           Keys.Image (New_Key (First)) &" to "&
                           Keys.Image (New_Key (Last)));

      for I in Integer'Max (First, 1) .. Integer'Min (Last, Loop_Count) loop
         Map.Search (New_Key (I), V, S);
         Assert (S = Failure, "Item in range still exists "& I'Img &" "&
                              Keys.Image (New_Key (I)));
      end loop;
   end;


   procedure Concurrent_Delete_Range (Map   : in out Maps.Map_Type'Class;
                                      First : in     Integer;
                                      Last  : in     Integer)
   is
      V : Values.Value_Type;
      S : State_Type;
   begin
      for I in Integer'Max (First, 1) .. Integer'Min (Last, Loop_Count) loop
         Map.Search (New_Key (I), V, S);
         Assert (S = Success, "Item in range doesn't exist "&
                              Keys.Image (New_Key (I)));
      end loop;

      declare
         task type Worker_Type is
            entry Start;
            entry Finish (State : out State_Type);
         end Worker_Type;

         task body Worker_Type is
         begin
            accept Start;
            Map.Delete_Range (New_Key (First), New_Key (Last), S);
            accept Finish (State : out State_Type) do
               State := S;
            end Finish;
         end Worker_Type;

         Tasks : array (Positive range 1 .. 5) of Worker_Type;
      begin
         for I in Tasks'Range loop
            Tasks (I).Start;
         end loop;
         for I in Tasks'Range loop
            Tasks (I).Finish (S);
            Assert (S = Success, "Delete range failed "&
                                 Keys.Image (New_Key (First)) &" to "&
                                 Keys.Image (New_Key (Last)));
         end loop;
      end;

      for I in Integer'Max (First, 1) .. Integer'Min (Last, Loop_Count) loop
         Map.Search (New_Key (I), V, S);
         Assert (S = Failure, "Item in range still exists "&
                              Keys.Image (New_Key (I)));
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
         K : Keys.Key_Type;
         V : Values.Value_Type;
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
         C.Delete (S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
      end;

      procedure Single_Cursor_Delete
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Keys.Key_Type;
         V : Values.Value_Type;
         S : State_Type;
         N : Natural := 0;
      begin
         C.Delete (S);
         Assert (S = Failure, "Cursor had no hit no value but Delete didn't "&
                              "fail");
         loop
            C.Next (K, V, S);
            exit when S /= Success;
            C.Delete (S);
            Assert (S = Success, "Deletion was not successful");
            N := N + 1;
         end loop;
         C.Delete (S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
         Assert (N = Loop_Count, "Met "& N'Img &" items where "&
                                 Loop_Count'Img &" were expected");
      end;

      procedure Single_Cursor_Pause_Delete (Expected : in Natural)
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Keys.Key_Type;
         V : Values.Value_Type;
         S : State_Type;
         N : Natural := 0;
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
            N := N + 1;
         end loop;
         C.Delete (S);
         Assert (S = Failure, "Cursor had finished but Delete didn't fail");
         Assert (N = Expected, "Met "& N'Img &" items where "&
                               Expected'Img &" were expected");
      end;

      procedure Half_Cursor_Delete (Expected : in Natural; Even : in Boolean)
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Keys.Key_Type;
         V : Values.Value_Type;
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
         Assert (N = Expected, "Met "& N'Img &" items where "&
                               Expected'Img &" were expected");
         Assert (M = Expected/2, "Deleted "& N'Img &" items where "&
                                 Expected'Img &" were expected");
      end;

      procedure Double_Cursor
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Keys.Key_Type;
         V : Values.Value_Type;
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
         K : Keys.Key_Type;
         V : Values.Value_Type;
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
      Create (Map, File_Name);
      Appends (Map);
      Single_Cursor_Pause;
      Single_Cursor_Delete;
      Empty;
      Appends (Map);
      Appends (Map);
      Double_Cursor;
      -- not 2*Loop_Count, because the recalibration will skip the duplicate
      Half_Cursor_Delete (Expected => Loop_Count, Even => True);
      Half_Cursor_Delete (Expected => Loop_Count, Even => False);
      Single_Cursor_Pause_Delete (Expected => Loop_Count);
      Empty;
      Finalize (Map);
      Test_Utils.Unlink (File_Name);
   exception
      when E: others =>
         Test_Utils.Unlink (File_Name);
         Put_Line (Exception_Information (E));
         raise;
   end;


   procedure Test_Range (T : in out Test_Type)
   is
      pragma Unreferenced (T);

      Map : Map_Type := New_Map;

      procedure Contains (M : Natural)
      is
         C : Maps.Cursor_Type'Class := Map.New_Cursor
           (False, Negative_Infinity_Bound, Positive_Infinity_Bound);
         K : Keys.Key_Type;
         V : Values.Value_Type;
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
         Assert (N = M, "Met "& N'Img &" items where"& M'Img &" were expected");
      end;

   begin
      Create (Map, File_Name);

      Appends (Map);
      Delete_Range (Map, 1, Loop_Count / 2);
      Contains (Loop_Count / 2);
      Delete_Range (Map, Loop_Count / 2 + 1, Loop_Count);
      Contains (0);

      Appends (Map);
      Delete_Range (Map, -100, Loop_Count / 2);
      Contains (Loop_Count / 2);
      Delete_Range (Map, Loop_Count / 2 + 1, Loop_Count + 100);
      Contains (0);

      Appends (Map);
      Delete_Range (Map, 1, Loop_Count - 1);
      Contains (1);
      Delete_Range (Map, Loop_Count, Loop_Count + 100);
      Contains (0);

      Appends (Map);
      Appends (Map);
      Concurrent_Delete_Range (Map, 1, Loop_Count);
      Contains (0);

      Finalize (Map);
      Test_Utils.Unlink (File_Name);
   exception
      when E: others =>
         Test_Utils.Unlink (File_Name);
         Put_Line (Exception_Information (E));
         raise;
   end;


end DB.Maps.Bounded.Test;

