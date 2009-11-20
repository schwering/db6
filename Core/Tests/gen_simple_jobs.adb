with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Random;

with DB.IO.Blocks;

package body Gen_Simple_Jobs is

   procedure Insert
   is
      KV    : constant Random.Key_Value_Type := Random.Random_Entry;
      Pos   : Count_Type;
      State : Result_Type := Success;
   begin
      P_Insert(Object, KV.Key, KV.Value, Pos, State);
      if State /= Success then
         Put_Line("Insertion failed "& State'Img);
         raise Stop_Now;
      end if;
   end Insert;


   procedure Delete
   is
      use type DB.IO.Blocks.Size_Type;
      use type DB.Types.Values.Value_Type;
      KV    : constant Random.Key_Value_Type := Random.Random_Entry;
      Val   : DB.Types.Values.Value_Type;
      Pos   : Count_Type;
      State : Result_Type := Success;
   begin
      P_Delete(Object, KV.Key, Val, Pos, State);
      if State /= Success or else KV.Value /= Val then
         Put_Line("Deletion failed "& State'Img);
         raise Stop_Now;
      end if;
   end Delete;


   procedure Search
   is
      use type DB.IO.Blocks.Size_Type;
      use type DB.Types.Values.Value_Type;
      KV     : constant Random.Key_Value_Type := Random.Random_Entry;
      Val    : DB.Types.Values.Value_Type;
      Pos    : Count_Type;
      State  : Result_Type := Success;
   begin
      P_Look_Up(Object, KV.Key, Val, Pos, State);
      if State /= Success or else KV.Value /= Val then
         Put_Line("Look up failed  "& State'Img);
         raise Stop_Now;
      end if;
   end Search;


   procedure Antisearch
   is
      use type DB.IO.Blocks.Size_Type;
      KV    : constant Random.Key_Value_Type := Random.Random_Entry;
      Val   : DB.Types.Values.Value_Type;
      Pos   : Count_Type;
      State : Result_Type := Failure;
   begin
      P_Look_Up(Object, KV.Key, Val, Pos, State);
      if State /= Failure then
         Put_Line("Look up failed "& State'Img);
         raise Stop_Now;
      end if;
   end Antisearch;

end Gen_Simple_Jobs;

