-- Abstract:
--
-- The composition of all test suites.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Locks.Mutexes.Test.Suite;
with DB.Maps.Bounded.Test.Suite;
with DB.Maps.Covering.Test.Suite;
with DB.Utils.Binary_Heaps.Test.Suite;
with DB.Utils.Regular_Expressions.Test.Suite;
with DB.Utils.Auto_Pointers.Test.Suite;
with DB.Utils.Smart_Pointers.Test.Suite;

package body Composite_Suite is

   function Suite return Access_Test_Suite
   is
      Res : constant Access_Test_Suite := New_Suite;
   begin
      Res.Add_Test (DB.Locks.Mutexes.Test.Suite.Suite);
      Res.Add_Test (DB.Utils.Binary_Heaps.Test.Suite.Suite);
      Res.Add_Test (DB.Utils.Regular_Expressions.Test.Suite.Suite);
      Res.Add_Test (DB.Utils.Auto_Pointers.Test.Suite.Suite);
      Res.Add_Test (DB.Utils.Smart_Pointers.Test.Suite.Suite);
      Res.Add_Test (DB.Maps.Bounded.Test.Suite.Suite);
      Res.Add_Test (DB.Maps.Covering.Test.Suite.Suite);
      return Res;
   end Suite;

end Composite_Suite;

