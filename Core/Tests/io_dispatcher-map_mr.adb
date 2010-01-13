with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with IO_Dispatcher.Map_Types;
with IO_Dispatcher.Args;
with IO_Dispatcher.Jobs;

with DB.Gen_Map_Reduce;
with DB.IO.Blocks;
with DB.IO.Blocks.Memory_IO;
with DB.Tables.Maps;
with DB.Types.Keys;
with DB.Types.Gen_Numbers;

with DB.Locks.Mutexes;

with DB.Utils.Traceback;
with DB.Utils.Global_Pool;

procedure IO_Dispatcher.Map_MR is

   package Maps renames DB.Tables.Maps;
   Map : Maps.Map_Type := Maps.New_Map(Map_Types.Max_Key_Size,
                                       Map_Types.Max_Value_Size);

   procedure Job
   is
      use type Map_Types.Key_Type;
      use type Map_Types.Value_Type;
      Lower       : constant Maps.Bound_Type
                  := Maps.Negative_Infinity_Bound(Map);
      Upper       : constant Maps.Bound_Type
                  := Maps.Positive_Infinity_Bound(Map);
      Trans       : Maps.RO_Transaction_Type
                  := Maps.New_RO_Transaction(Map);
      Cursor      : Maps.Cursor_Type
                  := Maps.New_Cursor(Map               => Map,
                                     Transaction       => Trans,
                                     Thread_Safe       => True,
                                     Lower_Bound       => Lower,
                                     Upper_Bound       => Upper,
                                     Reverse_Direction => False);

      subtype In_Key_Type is Map_Types.Key_Type;
      subtype In_Value_Type is Map_Types.Value_Type;

      package Intermediate_Values is new DB.Types.Gen_Numbers(Natural);
      subtype Intermediate_Key_Type is Map_Types.Key_Type;
      subtype Intermediate_Value_Type is Intermediate_Values.Number_Type;

      package Out_Values is new DB.Types.Gen_Numbers(Natural);
      subtype Out_Key_Type is Map_Types.Key_Type;
      subtype Out_Value_Type is Out_Values.Number_Type;

      procedure Input
        (Key     : out In_Key_Type;
         Value   : out In_Value_Type;
         Success : out Boolean)
      is
         use type Maps.State_Type;
         State : Maps.State_Type;
      begin
         Maps.Next(Map, Trans, Cursor, Key, Value, State);
         Success := State = Maps.Success;
         if State = Maps.Error then
            Put_Line("ERROR CURSOR");
         end if;
      end Input;

      procedure Map_Proc
        (Key   : in     In_Key_Type;
         Value : in     In_Value_Type;
         Emit  : access procedure (Key   : Intermediate_Key_Type;
                                   Value : Intermediate_Value_Type))
      is
         pragma Unreferenced (Value);
      begin
         Emit(Key, 1);
      exception
         when Error : others =>
            Put_Line("Exception: "& Exception_Message(Error));
            Put_Line("Exception: "& Exception_Information(Error));
            DB.Utils.Traceback.Print_Traceback(Error);
            raise;
      end Map_Proc;

      procedure Reduce
        (Key        : in     Intermediate_Key_Type;
         Next_Value : access procedure (Value   : out Intermediate_Value_Type;
                                        Success : out Boolean);
         Out_Key    :    out Out_Key_Type;
         Out_Value  :    out Out_Value_Type) is
      begin
         Out_Key   := Key;
         Out_Value := 0;
         loop
            declare
               Intermediate_Value : Intermediate_Value_Type;
               Success            : Boolean;
            begin
               Next_Value(Intermediate_Value, Success);
               exit when not Success;
               Out_Value := Out_Value + Intermediate_Value;
            end;
         end loop;
      end Reduce;

      Out_Mutex       : DB.Locks.Mutexes.Mutex_Type;
      Out_Initialized : Boolean := False;
      Last_Out_Key    : Out_Key_Type;
      Last_Out_Value  : Out_Value_Type;
      Out_Count       : Natural := 0;

      procedure Output
        (Key   : in Out_Key_Type;
         Value : in Out_Value_Type) is
      begin
         DB.Locks.Mutexes.Lock(Out_Mutex);
         if Out_Initialized then
            if not (Last_Out_Key <= Key) then
               Put_Line("Wrong output order: "&
                        Map_Types.To_String(Last_Out_Key) &
                        Map_Types.To_String(Key));
            end if;
            if Value /= Last_Out_Value then
               Put_Line("Differing output values:"&
                        Last_Out_Value'Img &" /="& Value'Img);
            end if;
            Out_Initialized := True;
         end if;
         Last_Out_Key   := Key;
         Last_Out_Value := Value;
         Out_Count := Out_Count + 1;
         --Put_Line(Map_Types.To_String(Key) &" =>"& Value'Img);
         DB.Locks.Mutexes.Unlock(Out_Mutex);
      exception
         when others =>
            DB.Locks.Mutexes.Unlock(Out_Mutex);
            raise;
      end Output;

      procedure Map_Reduce is new DB.Gen_Map_Reduce
        (In_Key_Type        => In_Key_Type,
         In_Value_Type      => In_Value_Type,
         Input              => Input,

         Intermediate_Key_Type           => Intermediate_Key_Type,
         Intermediate_Key_Context_Type   => DB.Types.Keys.Context_Type,
         Intermediate_Value_Type         => Intermediate_Value_Type,
         Intermediate_Value_Context_Type => Intermediate_Values.Context_Type,
         "="                             => Map_Types."=",
         "<="                            => Map_Types."<=",
         Key_Bound                       => Map_Types.Short_Bound,
         Key_Delimiter                   => Map_Types.Short_Delimiter,
         Intermediate_Key_Size_Bound     => DB.Types.Keys.Size_Bound,
         Read_Intermediate_Key           => DB.Types.Keys.Read,
         Skip_Intermediate_Key           => DB.Types.Keys.Skip,
         Write_Intermediate_Key          => DB.Types.Keys.Write,
         Intermediate_Value_Size_Bound   => Intermediate_Values.Size_Bound,
         Read_Intermediate_Value         => Intermediate_Values.Read,
         Skip_Intermediate_Value         => Intermediate_Values.Skip,
         Write_Intermediate_Value        => Intermediate_Values.Write,
         Intermediate_Block_IO           => DB.IO.Blocks.Memory_IO.IO,

         Map                => Map_Proc,
         Out_Key_Type       => Out_Key_Type,
         Out_Value_Type     => Out_Value_Type,
         Reduce             => Reduce,
         Output             => Output,
         Map_Task_Count     => 10,
         Reduce_Task_Count  => 10,
         Value_Queue_Size   => 100,
         Storage_Pool       => DB.Utils.Global_Pool.Global'Storage_Pool);

   begin
      Maps.Start_Transaction(Map, Trans);
      Map_Reduce;
      Maps.Finalize_Cursor(Map, Trans, Cursor);
      Maps.Finish_Transaction(Map, Trans);

      Put_Line("Count ="& Out_Count'Img);
      Put_Line("Value ="& Last_Out_Value'Img);
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

   Jobs.Execute_Job(Description             => Jobs.To_Description("MapReduce"),
                    Short_Job               => Job'Unrestricted_Access,
                    Short_Job_Execution_Count => 1,
                    Concurrency_Degree      => 1,
                    Reset                   => False);

   DB.Tables.Maps.Finalize(Map);
exception
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Utils.Traceback.Print_Traceback(Error);
end IO_Dispatcher.Map_MR;

