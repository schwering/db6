with Ada.Text_IO; use Ada.Text_IO;

with DB.IO.Blocks;

package body Gen_Simple_Jobs is

   procedure Insert
   is
      KV    : constant Key_Value_Type := Random_Entry;
      Pos   : Count_Type;
      State : Result_Type := Success;
   begin
      Check(KV);
      P_Insert(Object, Get_Key(KV), Get_Value(KV), Pos, State);
      if State /= Success then
         Put_Line("Insertion failed "& State'Img);
         raise Stop_Now;
      end if;
   end Insert;


   procedure Delete
   is
      use type DB.IO.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Random_Entry;
      Val   : Value_Type := Null_Value;
      Pos   : Count_Type;
      State : Result_Type := Success;
   begin
      P_Delete(Object, Get_Key(KV), Val, Pos, State);
      if State /= Success or else Get_Value(KV) /= Val then
         Put_Line("Deletion failed "& State'Img);
         raise Stop_Now;
      end if;
   end Delete;


   procedure Search
   is
      use type DB.IO.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Random_Entry;
      Val   : Value_Type := Null_Value;
      Pos   : Count_Type;
      State : Result_Type := Success;
   begin
      P_Look_Up(Object, Get_Key(KV), Val, Pos, State);
      if State /= Success or else Get_Value(KV) /= Val then
         Put_Line("Look up failed "& State'Img);
         raise Stop_Now;
      end if;
   end Search;


   procedure Antisearch
   is
      use type DB.IO.Blocks.Size_Type;
      KV    : constant Key_Value_Type := Random_Entry;
      Val   : Value_Type := Null_Value;
      Pos   : Count_Type;
      State : Result_Type := Failure;
   begin
      P_Look_Up(Object, Get_Key(KV), Val, Pos, State);
      if State /= Failure then
         Put_Line("Look up failed "& State'Img);
         raise Stop_Now;
      end if;
   end Antisearch;

end Gen_Simple_Jobs;

