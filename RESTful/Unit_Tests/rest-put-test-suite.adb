-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Caller;

package body REST.Put.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "REST.Put";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Flat", Test_Flat'Access));
      Ret.Add_Test (Caller.Create (Name & ": Nested", Test_Nested'Access));
      Ret.Add_Test (Caller.Create (Name & ": Flat Empty", Test_Flat_Empty'Access));
      Ret.Add_Test (Caller.Create (Name & ": Nested Empty", Test_Nested_Empty'Access));
      return Ret;
   end Suite;

end REST.Put.Test.Suite;

