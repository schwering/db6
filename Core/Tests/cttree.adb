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

with DB.CFS_BTrees;
with DB.Gen_BTrees.Gen_Check;
with DB.CFS_BTrees.Stats;

with DB.Types;
with DB.Types.Keys;
with DB.Types.Values;

with DB.Utils.Traceback;


procedure CTTree
is
   package BTrees renames DB.CFS_BTrees;

   Stop_Now : exception;

   subtype Key_Type is DB.Types.Keys.Key_Type;
   subtype Value_Type is DB.Types.Values.Value_Type;
   use type Key_Type;
   use type Value_Type;

   Tree : BTrees.Tree_Type;

   procedure Check
   is
      procedure Check_Proc is
         new BTrees.Gen_Check(Key_To_String     => To_Strings.To_String,
                              Value_To_String   => To_Strings.To_String,
                              Address_To_String => To_Strings.To_String);
   begin
      Check_Proc(Tree);
      Put_Line("Check successful");
   exception
      when Error : others =>
         Put_Line("Check failed");
         Put_Line("Exception: "& Exception_Message(Error));
         raise Stop_Now;
   end Check;


   procedure Stats
   is
      Count                                    : BTrees.Count_Type;
      Height, Blocks, Free_Blocks, Used_Blocks : Natural;
      Max_Degree, Min_Degree, Avg_Degree       : Natural;
      Waste, Waste_Per_Block, Bytes            : Long_Integer;
      Relative_Waste_Per_Block                 : Float;
   begin
      BTrees.Stats(Tree                   => Tree,
                   Height                 => Height,
                   Blocks                 => Blocks,
                   Free_Blocks            => Free_Blocks,
                   Max_Degree             => Max_Degree,
                   Min_Degree             => Min_Degree,
                   Avg_Degree             => Avg_Degree,
                   Bytes_Wasted_In_Blocks => Waste,
                   Bytes_In_Blocks        => Bytes);
      BTrees.Count(Tree, Count);
      Used_Blocks              := Blocks - Free_Blocks;
      if Used_Blocks > 0 then
         Waste_Per_Block       := Waste / Long_Integer(Used_Blocks);
      else
         Waste_Per_Block       := 0;
      end if;
      Relative_Waste_Per_Block := Float(Waste_Per_Block)
                                / Float(DB.IO.Blocks.Block_Size);
      -- The lines contain the following
      -- "OK" Count Height Blocks Used_Blocks Free_Blocks Max_Deg Avg_Dev\
      -- Min_Deg Total_Waste Total_Sizes Waste_per_Block Rel_Waste_per_Block
      Put("OK");
      Put(BTrees.Count_Type'Image(Count));
      Put(Natural'Image(Height));
      Put(Natural'Image(Blocks));
      Put(Natural'Image(Used_Blocks));
      Put(Natural'Image(Free_Blocks));
      Put(Natural'Image(Max_Degree));
      Put(Natural'Image(Avg_Degree));
      Put(Natural'Image(Min_Degree));
      Put(Long_Integer'Image(Waste));
      Put(Long_Integer'Image(Bytes));
      Put(Long_Integer'Image(Waste_Per_Block));
      Put(Float'Image(Relative_Waste_Per_Block));
      New_Line;
   end;


   package Simple_Jobs is new Gen_Simple_Jobs
     (BTrees.Tree_Type, BTrees.Count_Type, BTrees.Result_Type,
      Tree, BTrees.Success, BTrees.Failure,
      BTrees.Insert, BTrees.Delete, BTrees.Look_Up);

   Long_Job : constant Jobs.Long_Job_Type
            := Args.Create_Jobs_From_Command_Line(Simple_Jobs.Job_Map);
   Cnt      : BTrees.Count_Type := 0;
begin
   declare
   begin
      BTrees.Create(Args.File_Name);
      Put_Line("Newly created BTree "& Args.File_Name);
   exception
      when DB.IO_Error => Put_Line("Using existing BTree "& Args.File_Name);
   end;
   BTrees.Initialize(Tree, Args.File_Name);
   BTrees.Count(Tree, Cnt);
   Put_Line("Size ="& BTrees.Count_Type'Image(Cnt));

   declare
      RC : constant Random.Count_Type := Random.Count_Type(Cnt);
      IO : constant Random.Count_Type := Args.Init_Offset;
      I  : constant Count_Type := (RC - Random.Count_Type'Min(RC, IO)) * 10 + 1;
   begin
      Init_Key_Value_Pairs(I);
      Put_Line("Init ="& Count_Type'Image(I));
   end;

   Jobs.Execute_Jobs(Long_Job);

   BTrees.Finalize(Tree);

exception
   when Stop_Now =>
      null;
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Utils.Traceback.Print_Traceback(Error);
end CTTree;

