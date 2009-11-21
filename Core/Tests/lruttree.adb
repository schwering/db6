-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with System.Pool_Global; use System.Pool_Global;
with System.Storage_Pools; use System.Storage_Pools;

with Random; use Random;
with Args;
with Jobs;
with Gen_Simple_Jobs;

with DB;
with DB.IO.Blocks;
with DB.IO.Blocks.Direct_IO;
with DB.IO.Blocks.Gen_LRU_Caches;

with DB.Gen_BTrees;

with DB.Types;
with DB.Types.Keys;
with DB.Types.Values;

with DB.Utils.Traceback;


procedure LruTTree
is
   package LRU_Cache is new DB.IO.Blocks.Gen_LRU_Caches
     (DB.IO.Blocks.Direct_IO.IO);

   package Block_IO renames LRU_Cache.IO;

   package BTrees is new DB.Gen_BTrees
     (Key_Type           => DB.Types.Keys.Key_Type,
      Key_Context_Type   => DB.Types.Keys.Context_Type,
      Write_Key          => DB.Types.Keys.Write,
      Read_Key           => DB.Types.Keys.Read,
      Skip_Key           => DB.Types.Keys.Skip,
      "="                => DB.Types.Keys."=",
      "<="               => DB.Types.Keys."<=",
      Value_Type         => DB.Types.Values.Value_Type,
      Value_Context_Type => DB.Types.Values.Context_Type,
      Write_Value        => DB.Types.Values.Write,
      Read_Value         => DB.Types.Values.Read,
      Skip_Value         => DB.Types.Values.Skip,
      Is_Context_Free_Serialization =>
                              DB.Types.Keys.Is_Context_Free_Serialization
                          and DB.Types.Values.Is_Context_Free_Serialization,
      Storage_Pool       => Root_Storage_Pool'Class(Global_Pool_Object),
      Block_IO           => Block_IO);

   Tree : BTrees.Tree_Type;

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
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Utils.Traceback.Print_Traceback(Error);
end LruTTree;

