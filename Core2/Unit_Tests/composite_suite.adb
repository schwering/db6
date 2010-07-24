-- Abstract:
--
-- The composition of all test suites.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Maps.Bounded.Test.Suite;
with DB.Utils.Binary_Heaps.Test.Suite;
with DB.Utils.Regular_Expressions.Test.Suite;
with DB.Utils.Smart_Pointers.Test.Suite;

package body Composite_Suite is

   function Suite return Access_Test_Suite
   is
      Res : constant Access_Test_Suite := New_Suite;
   begin
      Res.Add_Test (DB.Maps.Bounded.Test.Suite.Suite);
      Res.Add_Test (DB.Utils.Binary_Heaps.Test.Suite.Suite);
      Res.Add_Test (DB.Utils.Regular_Expressions.Test.Suite.Suite);
      Res.Add_Test (DB.Utils.Smart_Pointers.Test.Suite.Suite);
      return Res;
   end Suite;

end Composite_Suite;

