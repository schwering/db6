-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with AUnit.Test_Caller;

package body DB.Maps.Values.Strings.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "Values.Strings";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Write/read",
                                   Test_Write_Read'Access));
      Ret.Add_Test (Caller.Create (Name & ": Size bound",
                                   Test_Size_Bound'Access));
      return Ret;
   end Suite;

end DB.Maps.Values.Strings.Test.Suite;

