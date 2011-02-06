-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Caller;

package body REST.Delete.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "REST.Delete";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Single", Test_Single'Access));
      Ret.Add_Test (Caller.Create (Name & ": Range", Test_Range'Access));
      return Ret;
   end Suite;

end REST.Delete.Test.Suite;

