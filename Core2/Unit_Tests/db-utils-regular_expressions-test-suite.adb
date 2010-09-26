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
      Name : constant String := "Regular_Expressions";
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Empty_Regexp", Test_Empty_Regexp'Access));
      Ret.Add_Test (Caller.Create (Name & ": Union", Test_Union'Access));
      Ret.Add_Test (Caller.Create (Name & ": Intersection", Test_Intersection'Access));
      Ret.Add_Test (Caller.Create (Name & ": Difference", Test_Difference'Access));
      --Ret.Add_Test (Caller.Create (Name & ": Union2", Test_Union2'Access));
      --Ret.Add_Test (Caller.Create (Name & ": Long", Test_Long'Access));
      return Ret;
   end Suite;

end DB.Utils.Regular_Expressions.Test.Suite;

