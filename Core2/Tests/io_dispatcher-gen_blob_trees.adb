with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;

with IO_Dispatcher.Test_Data; use IO_Dispatcher.Test_Data;
with IO_Dispatcher.Args;
with IO_Dispatcher.Jobs;
with IO_Dispatcher.Gen_Simple_Jobs;
with IO_Dispatcher.To_Strings; use IO_Dispatcher.To_Strings;

with DB;
with DB.Blocks;

with DB.Blob_Trees;

with DB.Types.Times;
with DB.Types.Values;
with DB.Types.Values.Bounded;
with DB.Types.Values.Unbounded;

procedure IO_Dispatcher.Gen_Blob_Trees is
   package Keys     renames DB.Blob_Trees.Keys;
   package Rows     renames Keys.Rows;
   --package Columns  renames Keys.Columns;
   package Values   renames DB.Blob_Trees.Values;

   Tree : Blob_Trees.Tree_Type;

   function To_Bounded
     (V : Test_Data.Values.String_Type)
      return DB.Types.Values.Bounded.String_Type is
   begin
      --return DB.Types.Values.Bounded.New_String(Test_Data.Values.To_Buffer(V));
      return V;
   end To_Bounded;

   function To_Unbounded
     (V : Test_Data.Values.String_Type)
      return DB.Types.Values.Unbounded.String_Type is
   begin
      return DB.Types.Values.Unbounded.New_String(Test_Data.Values.To_Buffer(V));
      --return V;
   end To_Unbounded;

   procedure Check_Key_Value (KV : Key_Value_Type)
   is
      use DB.Blocks;
      use type Size_Type;

      Key_Value_Error : exception;

      KS : constant DB.Blocks.Size_Type
         := 2 + Size_Type(Rows.Length(KV.Key.Row)) +
          --2 + Size_Type(Columns.Length(KV.Key.Column)) +
            Bits_To_Units(DB.Types.Times.Number_Type'Size);
   begin
      if KS > Blob_Trees.Max_Key_Size then
         raise Key_Value_Error;
      end if;
   end Check_Key_Value;


   function To_Key (K : Keys.Key_Type) return Blob_Trees.Key_Type
   is
      function Convert is new Ada.Unchecked_Conversion
         (Keys.Key_Type, Blob_Trees.Key_Type);
   begin
      return Convert(K);
   end To_Key;

   function To_Key (K : Blob_Trees.Key_Type) return Keys.Key_Type
   is
      function Convert is new Ada.Unchecked_Conversion
         (Blob_Trees.Key_Type, Keys.Key_Type);
   begin
      return Convert(K);
   end To_Key;

   function To_Value (V : Test_Data.Values.String_Type)
      return Blob_Trees.Value_Type
   is
      function Convert is new Ada.Unchecked_Conversion
         (Values.String_Type, Blob_Trees.Value_Type);
      --VV : aliased Blob_Trees.Value_Type;
      --for VV'Address use V'Address;
      --pragma Import (Ada, VV);
   begin
      return Convert(To_Unbounded(V));
      --return VV;
   end To_Value;

   function To_Value (V : Blob_Trees.Value_Type) return Values.String_Type
   is
      function Convert is new Ada.Unchecked_Conversion
         (Blob_Trees.Value_Type, Values.String_Type);
   begin
      return Convert(V);
   end To_Value;

   function Get_Key (KV : Key_Value_Type) return Blob_Trees.Key_Type is
   begin
      return To_Key(KV.Key);
   end Get_Key;

   function Get_Value (KV : Key_Value_Type) return Blob_Trees.Value_Type is
   begin
      return To_Value(KV.Value);
   end Get_Value;

   function Key_To_String (K : Blob_Trees.Key_Type) return String is
   begin
      return To_String(To_Key(K));
   end Key_To_String;

   function Value_To_String (V : Blob_Trees.Value_Type) return String is
   begin
      return To_String(To_Value(V));
   end Value_To_String;

   function "=" (Left, Right : Blob_Trees.Value_Type) return Boolean
   is
      use Values;
   begin
      return To_Value(Left) = To_Value(Right);
   end "=";

   Null_Value : Blob_Trees.Value_Type := To_Value(Test_Data.Values.Empty_String);

   procedure Stats
     (Tree                   : in out Blob_Trees.Tree_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer)
   is null;

   package Simple_Jobs is new Gen_Simple_Jobs
     (Object_Type     => Blob_Trees.Tree_Type,
      Key_Type        => Blob_Trees.Key_Type,
      Value_Type      => Blob_Trees.Value_Type,
      "="             => "=",
      Key_To_String   => Key_To_String,
      Value_To_String => Value_To_String,

      Key_Value_Type  => Key_Value_Type,
      Next_Entry      => Random_Entry,
      Get_Key         => Get_Key,
      Get_Value       => Get_Value,
      Check_Key_Value => Check_Key_Value,

      Count_Type      => Blob_Trees.Count_Type,
      State_Type      => Blob_Trees.State_Type,

      Object          => Tree,
      Null_Value      => Null_Value,
      Success         => Blob_Trees.Success,
      Failure         => Blob_Trees.Failure,

      P_Insert        => Blob_Trees.Insert,
      P_Delete        => Blob_Trees.Delete,
      P_Retrieve      => Blob_Trees.Retrieve,
      P_Count         => Blob_Trees.Count,
      P_Stats         => Stats,
      P_Check         => Check);


   use type Blob_Trees.State_Type;
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
      RC : constant Test_Data.Count_Type := Test_Data.Count_Type(Cnt);
      IO : constant Test_Data.Count_Type := Args.Init_Offset;
      I  : constant Count_Type := (RC - Test_Data.Count_Type'Min(RC, IO)) + 1;
   begin
      Init_Key_Value_Pairs(I);
      Put_Line("Init ="& Count_Type'Image(I));
   end;

   Jobs.Execute_Jobs(Long_Job);

   Blob_Trees.Finalize(Tree);
end IO_Dispatcher.Gen_Blob_Trees;

