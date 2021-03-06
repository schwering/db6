-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Caller;

package body DB.Locks.Mutexes.Test.Suite is

   package Caller is new AUnit.Test_Caller (Test_Type);

   function Suite return Access_Test_Suite
   is
      Name : constant String := "DB.Locks.Mutexes";
      Ret  : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Caller.Create (Name & ": Lock", Test_Lock'Access));
      Ret.Add_Test (Caller.Create (Name & ": Try_Lock", Test_Try_Lock'Access));
      return Ret;
   end Suite;

end DB.Locks.Mutexes.Test.Suite;

