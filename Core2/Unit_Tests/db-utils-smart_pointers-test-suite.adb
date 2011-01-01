-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with AUnit.Test_Caller;

package body DB.Utils.Smart_Pointers.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "Smart_Pointers";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Memory management",
                                   Test_Memory_Management'Access));
      return Ret;
   end Suite;

end DB.Utils.Smart_Pointers.Test.Suite;

