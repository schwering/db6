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
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create ("Register test",
                                   Test_Register'Access));
      Ret.Add_Test (Caller.Create ("Seal test",
                                   Test_Seal'Access));
      Ret.Add_Test (Caller.Create ("Map test",
                                   Test_Map'Access));
      return Ret;
   end Suite;

end DB.Maps.Tag_Map.Test.Suite;

