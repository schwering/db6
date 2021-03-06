-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with AUnit.Test_Caller;

package body DB.Maps.Bounded.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "Maps.Bounded";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Create",
                                   Test_Create'Access));
      Ret.Add_Test (Caller.Create (Name & ": Open",
                                   Test_Open'Access));
      Ret.Add_Test (Caller.Create (Name & ": Cursor",
                                   Test_Cursor'Access));
      Ret.Add_Test (Caller.Create (Name & ": Delete_Range",
                                   Test_Range'Access));
      return Ret;
   end Suite;

end DB.Maps.Bounded.Test.Suite;

