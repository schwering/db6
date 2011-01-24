-- Abstract:
--
-- Binary heap test fixture.
--
-- Copyright 2008--2011 Christoph Schwering

with AUnit.Test_Caller;
with AUnit.Test_Fixtures;

generic
   Initial_Key : Key_Type;
   with function Next_Key (Key : Key_Type) return Key_Type;
   with function Key_Img (Key : Key_Type) return String;
   Initial_Value : Value_Type;
   with function Next_Value (Value : Value_Type) return Value_Type;
   with function Value_Img (Value : Value_Type) return String;
package DB.DSA.Utils.Gen_Small_LRU_Caches.Gen_Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with
      record
         null;
      end record;

   overriding
   procedure Set_Up (T : in out Test_Type);

   overriding
   procedure Tear_Down (T : in out Test_Type);

   procedure Tests (T : in out Test_Type);


   -- The following usually goes to the suite, but for AI reasons we can't do
   -- that (the accesses are not allowed).

   package Caller is new AUnit.Test_Caller (Test_Type);

   Tests_Ref : Caller.Test_Method := Tests'Access;

end DB.DSA.Utils.Gen_Small_LRU_Caches.Gen_Test;

