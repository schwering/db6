-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Caller;

package body REST.Get.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "REST.Get";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Excl/Excl", Test_Excl_Excl'Access));
      Ret.Add_Test (Caller.Create (Name & ": Excl/Incl", Test_Excl_Incl'Access));
      Ret.Add_Test (Caller.Create (Name & ": Incl/Excl", Test_Incl_Excl'Access));
      Ret.Add_Test (Caller.Create (Name & ": Incl/Incl", Test_Excl_Incl'Access));
      Ret.Add_Test (Caller.Create (Name & ": Empty Excl/Excl", Test_Empty_Excl_Excl'Access));
      Ret.Add_Test (Caller.Create (Name & ": Empty Excl/Incl", Test_Empty_Excl_Incl'Access));
      Ret.Add_Test (Caller.Create (Name & ": Empty Incl/Excl", Test_Empty_Incl_Excl'Access));
      Ret.Add_Test (Caller.Create (Name & ": Empty Incl/Incl", Test_Empty_Excl_Incl'Access));
      Ret.Add_Test (Caller.Create (Name & ": Infinity", Test_Infinity'Access));
      Ret.Add_Test (Caller.Create (Name & ": Count", Test_Count'Access));
      return Ret;
   end Suite;

end REST.Get.Test.Suite;

