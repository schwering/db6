with Ada.Text_IO; use Ada.Text_IO;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Timers; use Timers;

procedure Controlled
is
   package AF renames Ada.Finalization;

   type Pointer is access Integer;
   procedure Free is new Ada.Unchecked_Deallocation(Integer, Pointer);

   package Controlled is
      type Controlled is new AF.Controlled with
         record
            Ptr : Pointer;
         end record;
      overriding procedure Finalize (O : in out Controlled);
   end Controlled;

   package body Controlled is
      overriding procedure Finalize (O : in out Controlled) is
      begin
         Free(O.Ptr);
      end Finalize;
   end Controlled;

   package Controlled_Finalization_Only is
      type Controlled is new AF.Controlled with
         record
            Ptr : Pointer;
         end record;
      pragma Finalize_Storage_Only (Controlled);
      overriding procedure Finalize (O : in out Controlled);
   end Controlled_Finalization_Only;

   package body Controlled_Finalization_Only is
      overriding procedure Finalize (O : in out Controlled) is
      begin
         Free(O.Ptr);
      end Finalize;
   end Controlled_Finalization_Only;

   procedure Calc (I : in out Integer) is
   begin
      I := I * I;
   end Calc;

   Cnt : constant := 10_000_000;
   T   : Timer_Type;

   procedure Run_Stack is
   begin
      Reset(T);
      Start(T);
      for I in 1 .. Cnt loop
         declare
            I : Integer;
         begin
            I := 1;
            Calc(I);
         end;
      end loop;
      Stop(T);
      --Print("raw", T);
      Print_Relative("stack", T, Cnt);
   end Run_Stack;

   procedure Run_Raw is
   begin
      Reset(T);
      Start(T);
      for I in 1 .. Cnt loop
         declare
            Ptr : Pointer;
         begin
            Ptr := new Integer'(1);
            Calc(Ptr.all);
            Free(Ptr);
         end;
      end loop;
      Stop(T);
      --Print("raw", T);
      Print_Relative("raw", T, Cnt);
   end Run_Raw;

   procedure Run_Controlled is
   begin
      Reset(T);
      Start(T);
      for I in 1 .. Cnt loop
         declare
            O : Controlled.Controlled;
         begin
            O.Ptr := new Integer'(1);
            Calc(O.Ptr.all);
         end;
      end loop;
      Stop(T);
      --Print("controlled", T);
      Print_Relative("controlled", T, Cnt);
   end Run_Controlled;

   procedure Run_Controlled_Finalization_Only is
   begin
      Reset(T);
      Start(T);
      for I in 1 .. Cnt loop
         declare
            O : Controlled_Finalization_Only.Controlled;
         begin
            O.Ptr := new Integer'(1);
            Calc(O.Ptr.all);
         end;
      end loop;
      Stop(T);
      --Print("controlled finalization only", T);
      Print_Relative("controlled finalization only", T, Cnt);
   end Run_Controlled_Finalization_Only;
begin
   Run_Stack;
   Run_Raw;
   Run_Controlled;
   Run_Controlled_Finalization_Only;
   Run_Controlled_Finalization_Only;
   Run_Controlled;
   Run_Raw;
   Run_Stack;
end Controlled;

