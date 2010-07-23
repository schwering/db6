-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Suites;

package Composite_Suite is
   use AUnit.Test_Suites;

   function Suite return Access_Test_Suite;

end Composite_Suite;

