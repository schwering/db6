-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Utils.Gen_Binary_Heaps.Gen_Test.Gen_Suite is

   function Suite return Access_Test_Suite
   is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create ("Test Binary_Heaps.Extract_Min",
                                   Test_Extract_Mins_Access));
      Ret.Add_Test (Caller.Create ("Test Binary_Heaps.Insert",
                                   Test_Inserts_Access));
      Ret.Add_Test (Caller.Create ("Test Binary_Heaps.Clear",
                                   Test_Clear_Access));
      Ret.Add_Test (Caller.Create ("Test Binary_Heaps.Is_Empty",
                                   Test_Is_Empty_Access));
      return Ret;
   end Suite;

end DB.Utils.Gen_Binary_Heaps.Gen_Test.Gen_Suite;

