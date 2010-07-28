-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Caller;

package body DB.Maps.Tag_Map.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "Value_Utils.String_Values";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Register",
                                   Test_Register'Access));
      Ret.Add_Test (Caller.Create (Name & ": Seal",
                                   Test_Seal'Access));
      Ret.Add_Test (Caller.Create (Name & ": Map",
                                   Test_Map'Access));
      return Ret;
   end Suite;

end DB.Maps.Tag_Map.Test.Suite;

