-- Abstract:
--
-- Binary heap test fixture.
--
-- Copyright 2008--2011 Christoph Schwering

with AUnit.Test_Caller;
with AUnit.Test_Fixtures;

generic
   Initial_Item : Item_Type;
   with function Succ (Item : Item_Type) return Item_Type;
   with function Img (Item : Item_Type) return String;
package DB.DSA.Utils.Gen_Binary_Heaps.Gen_Test is

   type Heap_Ref_Type is access Heap_Type;

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with
      record
         Last : Item_Type;
         Heap : Heap_Ref_Type;
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

end DB.DSA.Utils.Gen_Binary_Heaps.Gen_Test;

