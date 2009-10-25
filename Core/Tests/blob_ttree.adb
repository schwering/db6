-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with System.Pool_Global; use System.Pool_Global;
with System.Storage_Pools; use System.Storage_Pools;

with Random; use Random;
with Gen_Args;
with Gen_Jobs;

with DB.IO.Blocks;
with DB.IO.Blocks.Direct_IO;

with DB.Gen_Blob_Trees;

with DB.Types;
with DB.Types.Keys;
with DB.Types.Values;

with DB.Utils.Traceback;

with System.Storage_Elements;

procedure Blob_TTree
is
   subtype Key_Type is DB.Types.Keys.Key_Type;
   subtype Value_Type is DB.Types.Values.Value_Type;
   use type Key_Type;
   use type Value_Type;

   function To_Storage_Array
     (Value : Value_Type)
      return System.Storage_Elements.Storage_Array
   is
      Len    : constant DB.IO.Blocks.Position_Type
             := DB.IO.Blocks.Position_Type
                   (DB.IO.Blocks.Bits_To_Units(Value'Size));
      Cursor : DB.IO.Blocks.Cursor_Type;
      Block  : DB.IO.Blocks.Base_Block_Type(1 .. Len);
      procedure Write is new DB.IO.Blocks.Write(Value_Type);
   begin
      Write(Block, Cursor, Value);
      return System.Storage_Elements.Storage_Array(Block);
   end;

   function From_Storage_Array
     (Block : System.Storage_Elements.Storage_Array)
      return Value_Type
   is
      procedure Read is new DB.IO.Blocks.Read(Value_Type);
      Cursor : DB.IO.Blocks.Cursor_Type;
      Value  : Value_Type;
   begin
      Read(DB.IO.Blocks.Base_Block_Type(Block), Cursor, Value);
      return Value;
   end;

   package BTrees is new DB.Gen_Blob_Trees
     (Key_Type           => Key_Type,
      Key_Context_Type   => DB.Types.Keys.Context_Type,
      Write_Key          => DB.Types.Keys.Write,
      Read_Key           => DB.Types.Keys.Read,
      Skip_Key           => DB.Types.Keys.Skip,
      "="                => DB.Types.Keys."=",
      "<="               => DB.Types.Keys."<=",
      Value_Type         => Value_Type,
      To_Storage_Array   => To_Storage_Array,
      From_Storage_Array => From_Storage_Array,
      Is_Context_Free_Serialization =>
                            DB.Types.Keys.Is_Context_Free_Serialization,
      Storage_Pool       => Root_Storage_Pool'Class(Global_Pool_Object),
      Block_IO           => DB.IO.Blocks.Direct_IO.IO);

   package Jobs is new Gen_Jobs;
   package Args is new Gen_Args(Jobs);

   Tree : BTrees.Tree_Type;

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
            := ((Jobs.To_Description("Insert"),     Insert'Access),
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
end Blob_TTree;

