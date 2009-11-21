-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Random; use Random;
with To_Strings;
with Args;
with Jobs;
with Gen_Simple_Jobs;

with DB;
with DB.IO.Blocks;
with DB.IO.Blocks.File_IO;

with DB.Tables.Maps;

with DB.Types.Keys;
with DB.Types.Values;

with DB.Utils.Traceback;


procedure MMap
is
   package Maps renames DB.Tables.Maps;

   Max_Key_Size   : constant := 2 + 1000 + 8 
                                ;--+ 1; -- to enforce heaped map
   Max_Value_Size : constant := 8;

   Map : Maps.Map_Type := Maps.New_Map(Max_Key_Size, Max_Value_Size);

   package Simple_Jobs is new Gen_Simple_Jobs
     (Maps.Map_Type, Maps.Count_Type, Maps.Result_Type,
      Map, Maps.Success, Maps.Failure,
      Maps.Insert, Maps.Delete, Maps.Look_Up);

   use type Maps.Result_Type;
   Long_Job : constant Jobs.Long_Job_Type
            := Args.Create_Jobs_From_Command_Line(Simple_Jobs.Job_Map);
   Cnt      : Maps.Count_Type := 0;
begin
   declare
   begin
      Maps.Create(Args.File_Name, Max_Key_Size, Max_Value_Size);
      Put_Line("Newly created Map "& Args.File_Name);
   exception
      when DB.IO_Error => Put_Line("Using existing Map "& Args.File_Name);
   end;
   Maps.Initialize(Map, Args.File_Name);
   Maps.Count(Map, Cnt);
   Put_Line("Size ="& Maps.Count_Type'Image(Cnt));

   declare
      RC : constant Random.Count_Type := Random.Count_Type(Cnt);
      IO : constant Random.Count_Type := Args.Init_Offset;
      I  : constant Count_Type := (RC - Random.Count_Type'Min(RC, IO)) + 1;
   begin
      Init_Key_Value_Pairs(I);
      Put_Line("Init ="& Count_Type'Image(I));
   end;

   Jobs.Execute_Jobs(Long_Job);

   Maps.Finalize(Map);

   declare
      use DB.IO.Blocks.File_IO;
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create(File, Ada.Text_IO.Out_File, "block_access");
      for I in Blocks'Range loop
         Ada.Text_IO.Put_Line(File, I'Img &" = "& Blocks(I)'Img);
      end loop;
      Ada.Text_IO.Close(File);
   end;

exception
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Utils.Traceback.Print_Traceback(Error);
end MMap;

