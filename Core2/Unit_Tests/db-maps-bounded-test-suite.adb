-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Caller;

package body DB.Maps.Bounded.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "Maps.Bounded";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      return Ret;
   end Suite;

end DB.Maps.Bounded.Test.Suite;

