with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Random; use Random;
with This_Computer;
with To_Strings; use To_Strings;

with DB.BTrees;
with DB.Gen_BTrees.Gen_Check;

procedure Check
is
   package BTrees renames DB.BTrees;
   File_Name : constant String := This_Computer.BTree_File;

   procedure Check_Proc is
      new BTrees.Gen_Check(Key_To_String     => To_Strings.To_String,
                           Value_To_String   => To_Strings.To_String,
                           Address_To_String => To_Strings.To_String);
   Tree : BTrees.Tree_Type;
   Cnt  : BTrees.Count_Type;
begin
   BTrees.Initialize(Tree, File_Name);

   BTrees.Count(Tree, Cnt);
   Put_Line("Count:"& BTrees.Count_Type'Image(Cnt));
   Init_Key_Value_Pairs(Count_Type(Cnt)*10+1);
   Check_Proc(Tree);
   Put_Line("Check successful");

   BTrees.Finalize(Tree);

exception
   when Error : others =>
      Put_Line(Exception_Information(Error));
end Check;

