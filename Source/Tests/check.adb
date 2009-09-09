-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Random; use Random;
with This_Computer;
with To_Strings; use To_Strings;

with DB.BTrees;
with DB.Gen_BTrees.Gen_Check;

with DB.Util.Traceback;


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
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Util.Traceback.Print_Traceback;
      Put_Line("Traceback");
      DB.Util.Traceback.Print_Traceback(Error);
      Put_Line("Traceback");

end Check;

