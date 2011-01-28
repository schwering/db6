-- Abstract:
--
-- Time type in seconds. The 32 bits are enough for 136 years.
--
-- The implementation makes use of Ada.Calendar.Time and Ada.Calendar.Clock
-- which gives nanoseconds in a 64 bit type. This is enough for 584 years.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Types.Gen_Discretes;

package DB.Types.Times is
   pragma Elaborate_Body;

   type Time_Type is mod 2**32;

   package Serialization is new Gen_Discretes (Time_Type);

   function Now return Time_Type;

end DB.Types.Times;

