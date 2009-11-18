-- Abstract:
--
-- Timer for CPU and real time.
--
-- Design Notes:
--
-- See db-utils-timers-c.c for C functions.
--
-- Copyright 2008, 2009 Christoph Schwering

package DB.Utils.Timers is

   type Ticks_Type is mod 2**64;
   type Time_Type is mod 2**64;

   type Timer_Type is
      record
         Running         : Boolean    := False;

         CPU_Start_Time  : Ticks_Type := 0;
         CPU_Duration    : Ticks_Type := 0;

         Real_Start_Time : Time_Type := 0;
         Real_Duration   : Time_Type := 0;
      end record;


   procedure Start (Timer : in out Timer_Type);
   procedure Stop (Timer : in out Timer_Type);
   procedure Reset (Timer : out Timer_Type);
   function CPU_Ticks (Timer : Timer_Type) return Ticks_Type;
   function Real_Time (Timer : Timer_Type) return Time_Type;
   function CPU_String (Timer : Timer_Type) return String;
   function Real_String (Timer : Timer_Type) return String;
   procedure Print (S : in String; Timer : in out Timer_Type);

   Timer_Error : exception;

end DB.Utils.Timers;

