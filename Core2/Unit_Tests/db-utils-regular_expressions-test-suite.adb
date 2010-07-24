-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Caller;

package body DB.Utils.Regular_Expressions.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create ("Empty_Regexp test",
                                   Test_Empty_Regexp'Access));
      Ret.Add_Test (Caller.Create ("Union test",
                                   Test_Union'Access));
      Ret.Add_Test (Caller.Create ("Intersection test",
                                   Test_Intersection'Access));
      Ret.Add_Test (Caller.Create ("Difference test",
                                   Test_Difference'Access));
      return Ret;
   end Suite;

end DB.Utils.Regular_Expressions.Test.Suite;

