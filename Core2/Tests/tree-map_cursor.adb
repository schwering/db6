with Ada.Text_IO; use Ada.Text_IO;

with Tree.Test_Data;
with Tree.Types;
with Tree.Args;
with Tree.Jobs;

with DB.Maps;

procedure Tree.Map_Cursor is

   Map : DB.Maps.Map_Type'Class
       := DB.Maps.New_Map(Test_Data.Max_Key_Size,
                          Test_Data.Max_Value_Size);

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
      use type Types.Key_Type;
      use type Types.Value_Type;
      KV     : constant Types.Key_Value_Type := Test_Data.Random_Entry;
      Key    : constant Types.Key_Type   := KV.Key;
      Value  : constant Types.Value_Type := Types.Value(KV);
      LC     : constant DB.Maps.Comparison_Type  := DB.Maps.Equal;
      UC     : constant DB.Maps.Comparison_Type  := DB.Maps.Equal;
      Lower  : constant DB.Maps.Bound_Type := DB.Maps.New_Bound(LC, Key);
      Upper  : constant DB.Maps.Bound_Type := DB.Maps.New_Bound(UC, Key);
      Cursor : DB.Maps.Cursor_Type'Class
             := Map.New_Cursor(Thread_Safe => False,
                               Lower_Bound => Lower,
                               Upper_Bound => Upper);
      State  : DB.Maps.State_Type;
      Count  : Natural := 0;
   begin
      loop
         declare
            This_Key   : Types.Key_Type;
            This_Value : Types.Value_Type;
         begin
            Cursor.Next(This_Key, This_Value, State);
            case State is
               when DB.Maps.Success =>
                  pragma Assert (Key = This_Key);
                  pragma Assert (Value = This_Value);
                  Count := Count + 1;
               when DB.Maps.Failure =>
                  exit;
               when DB.Maps.Error =>
                  Ada.Text_IO.Put_Line("Error");
                  exit;
            end case;
         end;
      end loop;
      Cursor.Finalize_Cursor;
      Count_Container.Found(Count);
   end Job;

   Cnt : DB.Maps.Count_Type;
begin
   declare
   begin
      Map.Create(Args.File_Name);
      Ada.Text_IO.Put_Line("Newly created Map "& Args.File_Name);
   exception
      when DB.IO_Error =>
         Map.Initialize(Args.File_Name);
         Put_Line("Using existing Map "& Args.File_Name);
   end;
   Map.Count(Cnt);
   Ada.Text_IO.Put_Line("Size ="& DB.Maps.Count_Type'Image(Cnt));

   declare
      use type Types.Count_Type;
      RC : constant Types.Count_Type := Types.Count_Type(Cnt);
      IO : constant Types.Count_Type := Args.Init_Offset;
      I  : constant Types.Count_Type := (RC - Types.Count_Type'Min(RC, IO))+1;
   begin
      Test_Data.Init_Key_Value_Pairs(Args.Generator, I);
      Ada.Text_IO.Put_Line("Init ="& Types.Count_Type'Image(I));
   end;

   Jobs.Execute_Job(Description               => Jobs.To_Description("Cursor"),
                    Short_Job                 => Job'Unrestricted_Access,
                    Short_Job_Execution_Count => 1000,
                    Concurrency_Degree        => 10,
                    Reset                     => False);
   Ada.Text_IO.Put_Line("Total:"& Count_Container.Total_Count'Img);

   Map.Finalize;
end Tree.Map_Cursor;

