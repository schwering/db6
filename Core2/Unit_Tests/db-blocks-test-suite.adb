-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Caller;

package body DB.Blocks.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "DB.Blocks";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Cursor", Test_Cursor'Access));
      Ret.Add_Test (Caller.Create (Name & ": Utils", Test_Utils'Access));
      return Ret;
   end Suite;

end DB.Blocks.Test.Suite;

