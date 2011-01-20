-- Abstract:
--
-- The composition of all test suites.
--
-- Copyright 2008--2011 Christoph Schwering

with REST.Input_Formats.JSON.Test.Suite;

package body Composite_Suite is

   function Suite return Access_Test_Suite
   is
      Res : constant Access_Test_Suite := New_Suite;
   begin
      Res.Add_Test (REST.Input_Formats.JSON.Test.Suite.Suite);
      return Res;
   end Suite;

end Composite_Suite;
