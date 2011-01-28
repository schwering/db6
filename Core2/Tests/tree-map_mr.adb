with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Tree.Test_Data;
with Tree.Types;
with Tree.Args;
with Tree.Jobs;
with Tree.To_Strings;

with DB.DSA.Map_Reduce;
with DB.Blocks;
with DB.Blocks.Gen_Keys_Signature;
with DB.Blocks.Gen_Values_Signature;
with DB.Blocks.Local_IO;
with DB.Maps;
with DB.Types.Keys;
with DB.Types.Gen_Discretes;

with DB.Locks.Mutexes;

with DB.Utils.Global_Pool;

procedure Tree.Map_MR is

   Map : DB.Maps.Map_Type'Class
       := DB.Maps.New_Map(Test_Data.Max_Key_Size,
                          Test_Data.Max_Value_Size);

   procedure Job
   is
      Lower       : constant DB.Maps.Bound_Type
                  := DB.Maps.Negative_Infinity_Bound;
      Upper       : constant DB.Maps.Bound_Type
                  := DB.Maps.Positive_Infinity_Bound;
      Cursor      : DB.Maps.Cursor_Type'Class
                  := Map.New_Cursor(Thread_Safe => True,
                                    Lower_Bound => Lower,
                                    Upper_Bound => Upper);

      package Ins is
         subtype In_Key_Type is Types.Key_Type;
         subtype In_Value_Type is Types.Value_Type;
      end Ins;

      package Intermediates is
         use DB.Types.Keys;
         use DB.Types.Keys.Rows.Uncompressed;

         subtype Intermediate_Key_Type is DB.Types.Keys.Rows.String_Type;
         subtype Intermediate_Value_Type is Natural;

         package Intermediate_Keys is new DB.Blocks.Gen_Keys_Signature
           (Key_Type           => Intermediate_Key_Type,
            Read_Context_Type  => Rows.Uncompressed.Read_Context_Type,
            Write_Context_Type => Rows.Uncompressed.Write_Context_Type,
            Compare            => Rows.Compare);

         package Value_IO is new DB.Types.Gen_Discretes (Intermediate_Value_Type);
         use Value_IO;

         package Intermediate_Values is new DB.Blocks.Gen_Values_Signature
           (Value_Type         => Intermediate_Value_Type,
            Read_Context_Type  => Value_IO.Read_Context_Type,
            Write_Context_Type => Value_IO.Write_Context_Type);
      end Intermediates;

      package Outs is
         use DB.Types.Keys.Rows.Uncompressed;

         subtype Out_Key_Type is DB.Types.Keys.Rows.String_Type;
         subtype Out_Value_Type is Natural;
      end Outs;

      use Ins;
      use Intermediates;
      use Outs;

      use type In_Key_Type;
      use type In_Value_Type;
      use type Out_Key_Type;
      use type Out_Value_Type;

      procedure Input
        (Key     : out In_Key_Type;
         Value   : out In_Value_Type;
         Success : out Boolean)
      is
         use type DB.Maps.State_Type;
         State : DB.Maps.State_Type;
      begin
         Cursor.Next(Key, Value, State);
         Success := State = DB.Maps.Success;
      end Input;

      procedure Map_Proc
        (Key   : in     In_Key_Type;
         Value : in     In_Value_Type;
         Emit  : access procedure (Key   : Intermediate_Key_Type;
                                   Value : Intermediate_Value_Type))
      is
         pragma Unreferenced (Value);
      begin
         Emit(Key.Row, 1);
      exception
         when Error : others =>
            Put_Line("Exception: "& Exception_Information(Error));
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
         pragma Assert (Out_Value > 0);
      end Reduce;

      Out_Mutex       : DB.Locks.Mutexes.Mutex_Type;
      Out_Initialized : Boolean := False;
      Last_Out_Key    : Out_Key_Type;
      Last_Out_Value  : Out_Value_Type;
      Out_Value_Sum   : Out_Value_Type := 0;
      Out_Count       : Natural := 0;

      procedure Output
        (Key   : in Out_Key_Type;
         Value : in Out_Value_Type) is
      begin
         DB.Locks.Mutexes.Lock(Out_Mutex);
         if Out_Initialized then
            if not (Last_Out_Key <= Key) then
               Put_Line("Wrong output order: "&
                        To_Strings.To_String(Last_Out_Key) &
                        To_Strings.To_String(Key));
            end if;
            if Value /= Last_Out_Value then
               Put_Line("Differing output values:"&
                        Last_Out_Value'Img &" /="& Value'Img);
            end if;
         end if;
         Out_Initialized := True;
         Last_Out_Key    := Key;
         Last_Out_Value  := Value;
         Out_Value_Sum   := Out_Value_Sum + Value;
         Out_Count       := Out_Count + 1;
         --Put_Line(Types.To_String(Key) &" =>"& Value'Img);
         DB.Locks.Mutexes.Unlock(Out_Mutex);
      exception
         when others =>
            DB.Locks.Mutexes.Unlock(Out_Mutex);
            raise;
      end Output;

      procedure Map_Reduce is new DB.DSA.Map_Reduce.Gen_Local_Map_Reduce
        (In_Key_Type        => In_Key_Type,
         In_Value_Type      => In_Value_Type,
         Input              => Input,

         Intermediate_Keys     => Intermediate_Keys,
         Intermediate_Values   => Intermediate_Values,
         Intermediate_Block_IO => DB.Blocks.Local_IO.IO_Signature,

         Map                => Map_Proc,
         Out_Key_Type       => Out_Key_Type,
         Out_Value_Type     => Out_Value_Type,
         Reduce             => Reduce,
         Output             => Output,
         Map_Task_Count     => 10,
         Reduce_Task_Count  => 10,
         Value_Queue_Size   => 100,
         Storage_Pool       => DB.Utils.Global_Pool.Global_Storage_Pool);

   begin
      Map_Reduce("bluhp.intermediates");

      Put_Line("Count ="& Out_Count'Img);
      Put_Line("Value Sum ="& Out_Value_Sum'Img);
      Put_Line("Value ="& Last_Out_Value'Img);
   end Job;

   Cnt : DB.Maps.Count_Type;
begin
   declare
   begin
      Map.Create(Args.File_Name);
      Ada.Text_IO.Put_Line("Newly created Map "& Args.File_Name);
   exception
      when DB.IO_Error =>
         Map.Open(Args.File_Name);
         Put_Line("Using existing Map "& Args.File_Name);
   end;
   Map.Count(Cnt);
   Ada.Text_IO.Put_Line("Size ="& DB.Maps.Count_Type'Image(Cnt));

   Jobs.Execute_Job(Description             => Jobs.To_Description("MapReduce"),
                    Short_Job               => Job'Unrestricted_Access,
                    Short_Job_Execution_Count => 1,
                    Concurrency_Degree      => 1,
                    Reset                   => False);

   Map.Finalize;
end Tree.Map_MR;

