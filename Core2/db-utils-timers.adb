-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Text_IO;
with Interfaces.C;

package body DB.Utils.Timers is

   function C_clock return Interfaces.C.unsigned_long;
   pragma Import (C, C_clock, "db_utils_timer_clock");

   function C_time return Interfaces.C.unsigned_long;
   pragma Import (C, C_time, "db_utils_timer_time");

   C_CLOCKS_PER_SEC : constant Interfaces.C.unsigned_long;
   pragma Import (C, C_CLOCKS_PER_SEC, "db_utils_timer_CLOCKS_PER_SEC");

   C_TIMES_PER_SEC : constant Interfaces.C.unsigned_long;
   pragma Import (C, C_TIMES_PER_SEC, "db_utils_timer_TIMES_PER_SEC");

   CLOCKS_PER_SEC : constant Ticks_Type := Ticks_Type (C_CLOCKS_PER_SEC);
   TIMES_PER_SEC : constant Time_Type := Time_Type (C_TIMES_PER_SEC);


   procedure Start (Timer : in out Timer_Type) is
   begin
      if Timer.Running then
         raise Timer_Error;
      end if;

      Timer.Running := True;
      Timer.CPU_Start_Time := Ticks_Type (C_clock);
      Timer.Real_Start_Time := Time_Type (C_time);
   end Start;


   procedure Stop (Timer : in out Timer_Type) is
   begin
      if not Timer.Running then
         raise Timer_Error;
      end if;

      Timer.Running := False;

      declare
         Now : constant Ticks_Type := Ticks_Type (C_clock);
      begin
         Timer.CPU_Duration := (Now - Timer.CPU_Start_Time)
                              + Timer.CPU_Duration;
      end;

      declare
         Now : constant Time_Type := Time_Type (C_time);
      begin
         if Now >= Timer.Real_Start_Time then
            Timer.Real_Duration := (Now - Timer.Real_Start_Time)
                                  + Timer.Real_Duration;
         else
            Timer.Real_Duration := 0;
         end if;
      end;
   end Stop;


   procedure Reset (Timer : out Timer_Type) is
   begin
      Timer.Running         := False;

      Timer.CPU_Start_Time  := 0;
      Timer.CPU_Duration    := 0;

      Timer.Real_Start_Time := 0;
      Timer.Real_Duration   := 0;
   end Reset;


   function CPU_Ticks (Timer : Timer_Type) return Ticks_Type is
   begin
      if Timer.Running then
         raise Timer_Error;
      end if;

      declare
      begin
         return Timer.CPU_Duration;
      end;
   end CPU_Ticks;


   function Real_Time (Timer : Timer_Type) return Time_Type is
   begin
      if Timer.Running then
         raise Timer_Error;
      end if;

      declare
      begin
         return Timer.Real_Duration;
      end;
   end Real_Time;


   function CPU_String (Timer : Timer_Type) return String is
   begin
      declare
         T : constant Ticks_Type := CPU_Ticks (Timer);
         M : constant Natural := Natural (T / CLOCKS_PER_SEC / 60);
         S : constant Natural := Natural ((T / CLOCKS_PER_SEC) mod 60);
      begin
         return Natural'Image (M) &"m"& Natural'Image (S) &"s";
      end;
   exception
      when Constraint_Error =>
         return "0m0s";
   end CPU_String;


   function Real_String (Timer : Timer_Type) return String is
   begin
      declare
         T : constant Time_Type := Real_Time (Timer);
         M : constant Natural := Natural (T / TIMES_PER_SEC / 60);
         S : constant Natural := Natural ((T / TIMES_PER_SEC) mod 60);
      begin
         return Natural'Image (M) &"m"& Natural'Image (S) &"s";
      end;
   exception
      when Constraint_Error =>
         return "0m0s";
   end Real_String;


   procedure Print (S : in String; Timer : in Timer_Type)
   is
      function Trim (S : String) return String is
      begin
         if S (S'First) = ' ' then
            return S (S'First+1 .. S'Last);
         else
            return S;
         end if;
      end Trim;

      CPU_Duration  : constant Ticks_Type := CPU_Ticks (Timer);
      Real_Duration : constant Time_Type := Real_Time (Timer);
   begin
      Ada.Text_IO.Put (S &": ");
      Ada.Text_IO.Put (CPU_String (Timer) &" (");
      Ada.Text_IO.Put (Trim (Ticks_Type'Image (CPU_Duration)));
      Ada.Text_IO.Put (")   ");
      Ada.Text_IO.Put (" ");
      Ada.Text_IO.Put (Real_String (Timer) &" (");
      Ada.Text_IO.Put (Trim (Time_Type'Image (Real_Duration)));
      Ada.Text_IO.Put (")");
      Ada.Text_IO.New_Line;
   end Print;

end DB.Utils.Timers;

