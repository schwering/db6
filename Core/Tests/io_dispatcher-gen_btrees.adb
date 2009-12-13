with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;

with IO_Dispatcher.Random; use IO_Dispatcher.Random;
with IO_Dispatcher.Args;
with IO_Dispatcher.Jobs;
with IO_Dispatcher.Gen_Simple_Jobs;
with IO_Dispatcher.To_Strings; use IO_Dispatcher.To_Strings;

with DB;
with DB.IO.Blocks;

with DB.BTrees;

with DB.Types.Keys;
with DB.Types.Times;
with DB.Types.Values;
with DB.Types.Values.Bounded;
with DB.Types.Values.Unbounded;

with DB.Utils.Traceback;

procedure IO_Dispatcher.Gen_BTrees is
   package Keys     renames DB.BTrees.Keys;
   package Rows     renames Keys.Rows;
   package Columns  renames Keys.Columns;
   package Values   renames DB.BTrees.Values;

   Tree : BTrees.Tree_Type;

   function To_Bounded
     (V : Random.Values.String_Type)
      return DB.Types.Values.Bounded.String_Type is
   begin
      --return DB.Types.Values.Bounded.New_String(Random.Values.To_Buffer(V));
      return V;
   end To_Bounded;

   function To_Unbounded
     (V : Random.Values.String_Type)
      return DB.Types.Values.Unbounded.String_Type is
   begin
      return DB.Types.Values.Unbounded.New_String(Random.Values.To_Buffer(V));
      --return V;
   end To_Unbounded;

   procedure Check_Key_Value (KV : Key_Value_Type)
   is
      use DB.IO.Blocks;
      use type Size_Type;

      Key_Value_Error : exception;

      KS : constant DB.IO.Blocks.Size_Type
         := 2 + Size_Type(Rows.Length(KV.Key.Row)) +
          --2 + Size_Type(Columns.Length(KV.Key.Column)) +
            Bits_To_Units(DB.Types.Times.Number_Type'Size);
      VS : constant DB.IO.Blocks.Size_Type
         := Size_Type(Random.Values.Length(KV.Value));
   begin
      if KS > BTrees.Max_Key_Size(VS) then
         raise Key_Value_Error;
      end if;
   end Check_Key_Value;


   function To_Key (K : Keys.Key_Type) return BTrees.Key_Type
   is
      function Convert is new Ada.Unchecked_Conversion
         (Keys.Key_Type, BTrees.Key_Type);
      --KK : aliased BTrees.Key_Type;
      --for KK'Address use K'Address;
      --pragma Import (Ada, KK);
   begin
      return Convert(K);
      --return KK;
   end To_Key;

   function To_Key (K : BTrees.Key_Type) return Keys.Key_Type
   is
      function Convert is new Ada.Unchecked_Conversion
         (BTrees.Key_Type, Keys.Key_Type);
   begin
      return Convert(K);
   end To_Key;

   function To_Value (V : Random.Values.String_Type)
      return BTrees.Value_Type
   is
      function Convert is new Ada.Unchecked_Conversion
         (Values.String_Type, BTrees.Value_Type);
   begin
      return Convert(To_Bounded(V));
   end To_Value;

   function To_Value (V : BTrees.Value_Type) return Values.String_Type
   is
      function Convert is new Ada.Unchecked_Conversion
         (BTrees.Value_Type, Values.String_Type);
   begin
      return Convert(V);
   end To_Value;

   function Get_Key (KV : Key_Value_Type) return BTrees.Key_Type is
   begin
      return To_Key(KV.Key);
   end Get_Key;

   function Get_Value (KV : Key_Value_Type) return BTrees.Value_Type is
   begin
      return To_Value(KV.Value);
   end Get_Value;

   function Key_To_String (K : BTrees.Key_Type) return String is
   begin
      return To_String(To_Key(K));
   end Key_To_String;

   function Value_To_String (V : BTrees.Value_Type) return String is
   begin
      return To_String(To_Value(V));
   end Value_To_String;

   function "=" (Left, Right : BTrees.Value_Type) return Boolean
   is
      use Values;
   begin
      return To_Value(Left) = To_Value(Right);
   end "=";

   Null_Value : BTrees.Value_Type := To_Value(Random.Values.Empty_String);

   package Simple_Jobs is new Gen_Simple_Jobs
     (Object_Type     => BTrees.Tree_Type,
      Key_Type        => BTrees.Key_Type,
      Value_Type      => BTrees.Value_Type,
      "="             => "=",
      Key_To_String   => Key_To_String,
      Value_To_String => Value_To_String,

      Key_Value_Type  => Key_Value_Type,
      Random_Entry    => Random_Entry,
      Get_Key         => Get_Key,
      Get_Value       => Get_Value,
      Check_Key_Value => Check_Key_Value,

      Count_Type      => BTrees.Count_Type,
      Result_Type     => BTrees.Result_Type,

      Object          => Tree,
      Null_Value      => Null_Value,
      Success         => BTrees.Success,
      Failure         => BTrees.Failure,

      P_Insert        => BTrees.Insert,
      P_Delete        => BTrees.Delete,
      P_Look_Up       => BTrees.Look_Up,
      P_Count         => BTrees.Count,
      P_Make_Stats    => Stats,
      P_Check         => Check);


   use type BTrees.Result_Type;
   Long_Job : constant Jobs.Long_Job_Type
            := Args.Create_Jobs_From_Command_Line(Simple_Jobs.Job_Map);
   Cnt      : BTrees.Count_Type := 0;
begin
   declare
   begin
      BTrees.Create(Args.File_Name);
      Put_Line("Newly created Tree "& Args.File_Name);
   exception
      when DB.IO_Error => Put_Line("Using existing Tree "& Args.File_Name);
   end;
   BTrees.Initialize(Tree, Args.File_Name);
   BTrees.Count(Tree, Cnt);
   Put_Line("Size ="& BTrees.Count_Type'Image(Cnt));

   declare
      RC : constant Random.Count_Type := Random.Count_Type(Cnt);
      IO : constant Random.Count_Type := Args.Init_Offset;
      I  : constant Count_Type := (RC - Random.Count_Type'Min(RC, IO)) + 1;
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
end IO_Dispatcher.Gen_BTrees;

