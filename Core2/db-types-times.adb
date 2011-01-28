-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Calendar;
with Ada.Calendar.Conversions;

package body DB.Types.Times is

   function Now return Time_Type is
   begin
      return Time_Type
        (Ada.Calendar.Conversions.To_Unix_Time (Ada.Calendar.Clock));
   end Now;

end DB.Types.Times;

