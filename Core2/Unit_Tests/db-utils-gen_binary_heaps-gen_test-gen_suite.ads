-- Abstract:
--
-- Binary heap test suite.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Suites;

generic
package DB.Utils.Gen_Binary_Heaps.Gen_Test.Gen_Suite is
   use AUnit.Test_Suites;

   function Suite return Access_Test_Suite;

end DB.Utils.Gen_Binary_Heaps.Gen_Test.Gen_Suite;

