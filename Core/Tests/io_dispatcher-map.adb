with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with IO_Dispatcher.Random; use IO_Dispatcher.Random;
with IO_Dispatcher.Args;
with IO_Dispatcher.Jobs;
with IO_Dispatcher.Gen_Simple_Jobs;
with IO_Dispatcher.Map_Types;

with DB.IO.Blocks;
with DB.Tables.Maps;
with DB.Tables.Maps.Stats;
with DB.Tables.Maps.Check;
with DB.Types.Keys;
with DB.Types.Strings.Bounded;
with DB.Types.Values.Bounded;
with DB.Types.Times;

procedure IO_Dispatcher.Map is

   Map : DB.Tables.Maps.Map_Type
       := DB.Tables.Maps.New_Map(Map_Types.Max_Key_Size,
                                 Map_Types.Max_Value_Size);

   procedure Check_Key_Value (KV : Random.Key_Value_Type)
   is
      use DB.IO.Blocks;
      use DB.Types.Strings.Bounded;
      pragma Warnings (Off);
      use DB.Types.Values.Bounded;
      pragma Warnings (On);
      use type Size_Type;

      function KS return DB.IO.Blocks.Size_Type is
      begin
         return 2 + Size_Type(Length(KV.Key.Row)) +
              --2 + Size_Type(Length(KV.Key.Column)) +
                Bits_To_Units(DB.Types.Times.Number_Type'Size);
      end;
      function VS return DB.IO.Blocks.Size_Type is
      begin
         return Size_Type(Length(KV.Value));
      end;
   begin
      pragma Assert (KS <= DB.Tables.Maps.Max_Key_Size(Map, VS));
      null;
   end Check_Key_Value;

   Null_Value : DB.Tables.Value_Type'Class := Map_Types.Null_Value;

   package Simple_Jobs is new Gen_Simple_Jobs
     (Object_Type     => DB.Tables.Maps.Map_Type,
      Key_Type        => Map_Types.Key_Type,
      Value_Type      => DB.Tables.Value_Type'Class,
 
      Key_To_String   => Map_Types.To_String,
      Value_To_String => Map_Types.To_String,

      "="             => DB.Tables."=",

      Check_Key_Value => Check_Key_Value,

      Key_Value_Type  => Random.Key_Value_Type,
      Random_Entry    => Random.Random_Entry,
      Get_Key         => Random.Key,
      Get_Value       => Map_Types.Get_Value,

      Count_Type      => DB.Tables.Maps.Count_Type,
      State_Type      => DB.Tables.Maps.State_Type,

      Object          => Map,
      Null_Value      => Null_Value,
      Success         => DB.Tables.Maps.Success,
      Failure         => DB.Tables.Maps.Failure,

      P_Insert        => DB.Tables.Maps.Insert,
      P_Delete        => DB.Tables.Maps.Delete,
      P_Retrieve       => DB.Tables.Maps.Retrieve,
      P_Count         => DB.Tables.Maps.Count,
      P_Stats         => DB.Tables.Maps.Stats,
      P_Check         => DB.Tables.Maps.Check);


   use type DB.Tables.Maps.State_Type;
   Job_Map  : constant Jobs.Map_Type := Simple_Jobs.Job_Map;
   Long_Job : constant Jobs.Long_Job_Type
            := Args.Create_Jobs_From_Command_Line(Job_Map);
   Cnt      : DB.Tables.Maps.Count_Type := 0;
begin
   declare
   begin
      DB.Tables.Maps.Create(Args.File_Name,
                            Map_Types.Max_Key_Size,
                            Map_Types.Max_Value_Size);
      Put_Line("Newly created Map "& Args.File_Name);
   exception
      when DB.IO_Error => Put_Line("Using existing Map "& Args.File_Name);
   end;
   DB.Tables.Maps.Initialize(Map, Args.File_Name);
   DB.Tables.Maps.Count(Map, Cnt);
   Put_Line("Size ="& DB.Tables.Maps.Count_Type'Image(Cnt));

   declare
      RC : constant Random.Count_Type := Random.Count_Type(Cnt);
      IO : constant Random.Count_Type := Args.Init_Offset;
      I  : constant Count_Type := (RC - Random.Count_Type'Min(RC, IO)) + 1;
   begin
      Init_Key_Value_Pairs(I);
      Put_Line("Init ="& Count_Type'Image(I));
   end;

   Jobs.Execute_Jobs(Long_Job);

   DB.Tables.Maps.Finalize(Map);
end IO_Dispatcher.Map;

