with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with System.Pool_Global; use System.Pool_Global;
with System.Storage_Pools; use System.Storage_Pools;

with IO_Dispatcher.Random; use IO_Dispatcher.Random;
with IO_Dispatcher.Args;
with IO_Dispatcher.Jobs;
with IO_Dispatcher.Gen_Simple_Jobs;
with IO_Dispatcher.To_Strings; use IO_Dispatcher.To_Strings;

with DB;
with DB.IO.Blocks;
with DB.Gen_Blob_Trees;

with DB.Types.Keys;
with DB.Types.Strings;
with DB.Types.Strings.Bounded;
with DB.Types.Values.Bounded;
with DB.Types.Times;

with DB.Utils.Traceback;

procedure IO_Dispatcher.Gen_Blob_Trees is
   package Keys     renames DB.Types.Keys;
   package Values   renames DB.Types.Values.Bounded;
   package Value_IO renames DB.Types.Values.Bounded.Uncompressed;
   package Blob_Trees is new DB.Gen_Blob_Trees
     (Key_Type           => Keys.Key_Type,
      Key_Context_Type   => Keys.Context_Type,
      Read_Key           => Keys.Read,
      Skip_Key           => Keys.Skip,
      Write_Key          => Keys.Write,
      "="                => Keys."=",
      "<="               => Keys."<=",
      Value_Type         => Values.String_Type,
      To_Storage_Array   => Value_IO.To_Storage_Array,
      From_Storage_Array => Value_IO.From_Storage_Array,
      Is_Context_Free_Serialization => 
                            Keys.Is_Context_Free_Serialization and
                            Value_IO.Is_Context_Free_Serialization,
      Storage_Pool       => Root_Storage_Pool'Class(Global_Pool_Object),
      Block_IO           => Block_IO);


   procedure Check_Key_Value (KV : Key_Value_Type)
   is
      use DB.IO.Blocks;
      use DB.Types.Strings.Bounded;
      use DB.Types.Values.Bounded;
      use type Size_Type;

      Key_Value_Error : exception;

      KS : constant DB.IO.Blocks.Size_Type
         := 2 + Size_Type(Length(KV.Key.Row)) +
          --2 + Size_Type(Length(KV.Key.Column)) +
            Bits_To_Units(DB.Types.Times.Number_Type'Size);
   begin
      if KS > Blob_Trees.Max_Key_Size then
         raise Key_Value_Error;
      end if;
   end Check_Key_Value;


   procedure Make_Stats
     (Tree                   : in out Blob_Trees.Tree_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer) is null;
   procedure Check (Tree : in out Blob_Trees.Tree_Type) is null;


   Tree : Blob_Trees.Tree_Type;

   package Simple_Jobs is new Gen_Simple_Jobs
     (Object_Type     => Blob_Trees.Tree_Type,
      Key_Type        => Keys.Key_Type,
      Value_Type      => Values.String_Type,

      Key_To_String   => To_String,
      Value_To_String => To_String,

      "="             => Values."=",

      Check_Key_Value => Check_Key_Value,

      Key_Value_Type  => Random.Key_Value_Type,
      Random_Entry    => Random.Random_Entry,
      Get_Key         => Random.Key,
      Get_Value       => Random.Value,

      Count_Type      => Blob_Trees.Count_Type,
      Result_Type     => Blob_Trees.Result_Type,

      Object          => Tree,
      Null_Value      => Values.Empty_String,
      Success         => Blob_Trees.Success,
      Failure         => Blob_Trees.Failure,

      P_Insert        => Blob_Trees.Insert,
      P_Delete        => Blob_Trees.Delete,
      P_Look_Up       => Blob_Trees.Look_Up,
      P_Count         => Blob_Trees.Count,
      P_Make_Stats    => Make_Stats,
      P_Check         => Check);


   use type Blob_Trees.Result_Type;
   Long_Job : constant Jobs.Long_Job_Type
            := Args.Create_Jobs_From_Command_Line(Simple_Jobs.Job_Map);
   Cnt      : Blob_Trees.Count_Type := 0;
begin
   declare
   begin
      Blob_Trees.Create(Args.File_Name);
      Put_Line("Newly created Tree "& Args.File_Name);
   exception
      when DB.IO_Error => Put_Line("Using existing Tree "& Args.File_Name);
   end;
   Blob_Trees.Initialize(Tree, Args.File_Name);
   Blob_Trees.Count(Tree, Cnt);
   Put_Line("Size ="& Blob_Trees.Count_Type'Image(Cnt));

   declare
      RC : constant Random.Count_Type := Random.Count_Type(Cnt);
      IO : constant Random.Count_Type := Args.Init_Offset;
      I  : constant Count_Type := (RC - Random.Count_Type'Min(RC, IO)) + 1;
   begin
      Init_Key_Value_Pairs(I);
      Put_Line("Init ="& Count_Type'Image(I));
   end;

   Jobs.Execute_Jobs(Long_Job);

   Blob_Trees.Finalize(Tree);
exception
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Utils.Traceback.Print_Traceback(Error);
end IO_Dispatcher.Gen_Blob_Trees;

