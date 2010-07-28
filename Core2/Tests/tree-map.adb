with Ada.Text_IO; use Ada.Text_IO;

with Tree.Args;
with Tree.Jobs;
with Tree.Gen_Simple_Jobs;
with Tree.Test_Data;
with Tree.Types;

with DB.Blocks;
with DB.Maps; use DB.Maps;

procedure Tree.Map is

   Map : DB.Maps.Map_Type'Class :=
      DB.Maps.New_Map (Args.Implementation);
    --DB.Maps.New_Map (Test_Data.Max_Key_Size,
    --                 Test_Data.Max_Value_Size);

   Null_Value : Types.Value_Type'Class := Types.Null_Value;

   package Simple_Jobs is new Gen_Simple_Jobs
     (Next_Entry      => Test_Data.Random_Entry,
      Map             => Map,
      Null_Value      => Null_Value);


   use type Types.Count_Type;
   Job_Map  : constant Jobs.Map_Type := Simple_Jobs.Job_Map;
   Long_Job : constant Jobs.Long_Job_Type
            := Args.Create_Jobs_From_Command_Line(Job_Map);
   Cnt      : Long_Integer := 0;
begin
   declare
   begin
      Map.Create(Args.File_Name);
      Put_Line("Newly created Map "& Args.File_Name);
   exception
      when DB.IO_Error =>
         Map.Open(Args.File_Name);
         Put_Line("Using existing Map "& Args.File_Name);
   end;
   DB.Maps.Count(Map, Cnt);
   Put_Line("Size ="& DB.Maps.Count_Type'Image(Cnt));

   declare
      RC : constant Types.Count_Type := Types.Count_Type(Cnt);
      IO : constant Types.Count_Type := Args.Init_Offset;
      I  : constant Types.Count_Type := (RC - Types.Count_Type'Min(RC, IO)) + 1;
   begin
      Test_Data.Init_Key_Value_Pairs(Args.Generator, I);
      Put_Line("Init ="& Types.Count_Type'Image(I));
   end;

   Jobs.Execute_Jobs(Long_Job);

   DB.Maps.Finalize(Map);
end Tree.Map;

