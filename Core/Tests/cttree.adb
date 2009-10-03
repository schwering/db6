-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
--with Ada.Command_Line; use Ada.Command_Line;

with Random; use Random;
with To_Strings;
with Gen_Args;
with Gen_Jobs;

with DB.IO.Blocks;

with DB.CFS_BTrees;
with DB.Gen_BTrees.Gen_Check;
with DB.CFS_BTrees.Stats;

with DB.Types;
with DB.Types.Keys;
with DB.Types.Values;

with DB.Util.Traceback;


procedure CTTree
is
   package BTrees renames DB.CFS_BTrees;
   package Jobs is new Gen_Jobs;
   package Args is new Gen_Args(Jobs);

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


   --Max_Size : DB.IO.Blocks.Size_Type := 0;
   procedure Insert
   is
      use type BTrees.Count_Type;
      use type BTrees.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Pos    : BTrees.Count_Type;
      State  : BTrees.Result_Type;
      --Size   : DB.IO.Blocks.Size_Type;
      --Context : DB.Types.Keys.Context_Type;
   begin
      --DB.Types.Keys.Get_Size_Of(Context, KV.Key, Size);
      --Put_Line("This key size ="& DB.IO.Blocks.Size_Type'Image(Size));
      --if Max_Size < Size then Max_Size := Size; end if;
      --Put_Line("Max inserted key size ="&
               --DB.IO.Blocks.Size_Type'Image(Max_Size));
      --Put_Line("Max possible key size ="&
               --DB.IO.Blocks.Size_Type'Image(BTrees.Max_Key_Size));
      BTrees.Insert(Tree, KV.Key, KV.Value, Pos, State);
      if State /= BTrees.Success then
         Put_Line("Insertion failed");
      end if;
   end Insert;


   procedure Delete
   is
      use type BTrees.Count_Type;
      use type BTrees.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Val    : Value_Type;
      Pos    : BTrees.Count_Type;
      State  : BTrees.Result_Type;
   begin
      BTrees.Delete(Tree, KV.Key, Val, Pos, State);
      if State /= BTrees.Success or else KV.Value /= Val then
         Put_Line("Deletion failed");
      end if;
   end Delete;


   procedure Search
   is
      use type BTrees.Count_Type;
      use type BTrees.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Val    : Value_Type;
      Pos    : BTrees.Count_Type;
      State  : BTrees.Result_Type;
   begin
      BTrees.Look_Up(Tree, KV.Key, Val, Pos, State);
      if State /= BTrees.Success or else KV.Value /= Val then
         Put_Line("Look up failed "& BTrees.Result_Type'Image(State));
      end if;
   end Search;


   procedure Antisearch
   is
      use type BTrees.Count_Type;
      use type BTrees.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Val    : Value_Type;
      Pos    : BTrees.Count_Type;
      State  : BTrees.Result_Type;
   begin
      BTrees.Look_Up(Tree, KV.Key, Val, Pos, State);
      if State /= BTrees.Failure then
         Put_Line("Look up failed");
      end if;
   end Antisearch;


   use type BTrees.Result_Type;
   Map      : constant Jobs.Map_Type
            := ((Jobs.To_Description("Stats"),      Stats'Access),
                (Jobs.To_Description("Check"),      Check'Access),
                (Jobs.To_Description("Insert"),     Insert'Access),
                (Jobs.To_Description("Delete"),     Delete'Access),
                (Jobs.To_Description("Search"),     Search'Access),
                (Jobs.To_Description("Antisearch"), Antisearch'Access));
   Long_Job : constant Jobs.Long_Job_Type
            := Args.Create_Jobs_From_Command_Line(Map);
   Cnt      : BTrees.Count_Type := 0;
begin
   declare
   begin
      BTrees.Create(Args.File_Name);
      Put_Line("Newly created BTree "& Args.File_Name);
   exception
      when DB.IO.IO_Error => Put_Line("Using existing BTree "& Args.File_Name);
   end;
   BTrees.Initialize(Tree, Args.File_Name);
   BTrees.Count(Tree, Cnt);
   Put_Line("Size ="& BTrees.Count_Type'Image(Cnt));

   declare
      I : constant Count_Type
        := (Random.Count_Type(Cnt) - Args.Init_Offset) * 10 + 1;
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
      DB.Util.Traceback.Print_Traceback(Error);
end CTTree;

