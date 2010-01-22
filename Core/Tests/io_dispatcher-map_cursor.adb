with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with IO_Dispatcher.Random;
with IO_Dispatcher.Map_Types;
with IO_Dispatcher.Args;
with IO_Dispatcher.Jobs;

with DB.Tables.Maps;

procedure IO_Dispatcher.Map_Cursor is

   package Maps renames DB.Tables.Maps;
   Map : Maps.Map_Type := Maps.New_Map(Map_Types.Max_Key_Size,
                                       Map_Types.Max_Value_Size);

   protected Count_Container is
      procedure Found (C : in Natural);
      function Total_Count return Natural;
   private
      Initialized : Boolean := False;
      Count       : Natural;
      Total       : Natural := 0;
   end Count_Container;

   protected body Count_Container is
      procedure Found (C : in Natural) is
      begin
         if not Initialized then
            Count       := C;
            Initialized := True;
            Ada.Text_IO.Put_Line(Count'Img &" entries");
         else
            if C /= Count then
               Ada.Text_IO.Put_Line("Error:"& C'Img &" /="& Count'Img);
            end if;
         end if;
         Total := Total + C;
      end Found;

      function Total_Count return Natural is
      begin
         return Total;
      end Total_Count;
   end Count_Container;

   procedure Job
   is
      use type Map_Types.Key_Type;
      use type Map_Types.Value_Type;
      KV          : constant Random.Key_Value_Type := Random.Random_Entry;
      Key         : constant Map_Types.Key_Type    := KV.Key;
      Value       : constant Map_Types.Value_Type 
                  := Map_Types.From_Bounded(KV.Value);
      LC          : constant Maps.Comparison_Type  := Maps.Equal;
      UC          : constant Maps.Comparison_Type  := Maps.Equal;
      Lower       : constant Maps.Bound_Type := Maps.New_Bound(Map, LC, Key);
      Upper       : constant Maps.Bound_Type := Maps.New_Bound(Map, UC, Key);
      Trans       : Maps.RO_Transaction_Type := Maps.New_RO_Transaction(Map);
      Cursor      : Maps.Cursor_Type
                  := Maps.New_Cursor(Map               => Map,
                                     Transaction       => Trans,
                                     Thread_Safe       => False,
                                     Lower_Bound       => Lower,
                                     Upper_Bound       => Upper,
                                     Reverse_Direction => False);
      State       : Maps.State_Type;
      Count       : Natural := 0;
   begin
      Maps.Start_Transaction(Map, Trans);
      loop
         declare
            This_Key   : Map_Types.Key_Type;
            This_Value : Map_Types.Value_Type;
         begin
            Maps.Next(Map, Trans, Cursor, This_Key, This_Value, State);
            case State is
               when Maps.Success =>
                  pragma Assert (Key = This_Key);
                  pragma Assert (Value = This_Value);
                  Count := Count + 1;
               when Maps.Failure =>
                  exit;
               when Maps.Error =>
                  Ada.Text_IO.Put_Line("Error");
                  exit;
            end case;
         end;
      end loop;
      Maps.Finalize_Cursor(Map, Trans, Cursor);
      Maps.Finish_Transaction(Map, Trans);
      Count_Container.Found(Count);
   end Job;

   Cnt : Maps.Count_Type;
begin
   declare
   begin
      DB.Tables.Maps.Create(Args.File_Name, Map_Types.Max_Key_Size,
                            Map_Types.Max_Value_Size);
      Ada.Text_IO.Put_Line("Newly created Map "& Args.File_Name);
   exception
      when DB.IO_Error => Put_Line("Using existing Map "& Args.File_Name);
   end;
   Maps.Initialize(Map, Args.File_Name);
   Maps.Count(Map, Cnt);
   Ada.Text_IO.Put_Line("Size ="& Maps.Count_Type'Image(Cnt));

   declare
      use type Maps.Count_Type;
      use type Random.Count_Type;
      RC : constant Random.Count_Type := Random.Count_Type(Cnt);
      IO : constant Random.Count_Type := Args.Init_Offset;
      I  : constant Random.Count_Type := (RC - Random.Count_Type'Min(RC, IO))+1;
   begin
      Random.Init_Key_Value_Pairs(I);
      Ada.Text_IO.Put_Line("Init ="& Random.Count_Type'Image(I));
   end;

   Jobs.Execute_Job(Description               => Jobs.To_Description("Cursor"),
                    Short_Job                 => Job'Unrestricted_Access,
                    Short_Job_Execution_Count => 1000,
                    Concurrency_Degree        => 10,
                    Reset                     => False);
   Ada.Text_IO.Put_Line("Total:"& Count_Container.Total_Count'Img);

   DB.Tables.Maps.Finalize(Map);
end IO_Dispatcher.Map_Cursor;

