-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Utils.Gen_Binary_Heaps.Gen_Test.Gen_Suite is

   function Suite return Access_Test_Suite
   is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create ("Binary_Heaps", Tests_Ref));
      return Ret;
   end Suite;

end DB.Utils.Gen_Binary_Heaps.Gen_Test.Gen_Suite;

