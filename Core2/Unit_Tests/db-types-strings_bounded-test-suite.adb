-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Caller;

package body DB.Types.Strings_Bounded.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "DB.Types.Strings.Bounded";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Hash", Test_Hash'Access));
      return Ret;
   end Suite;

end DB.Types.Strings_Bounded.Test.Suite;

