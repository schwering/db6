-- Abstract:
--
-- Binary heap test fixture.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Caller;
with AUnit.Test_Fixtures;

generic
   Initial_Item : Item_Type;
   with function Succ (Item : Item_Type) return Item_Type;
   with function Img (Item : Item_Type) return String;
package DB.Utils.Gen_Binary_Heaps.Gen_Test is

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

   procedure Test_Clear (T : in out Test_Type);
   procedure Test_Is_Empty (T : in out Test_Type);

   procedure Test_Extract_Min (T : in out Test_Type);
   procedure Test_Insert (T : in out Test_Type);

   procedure Test_Inserts (T : in out Test_Type);
   procedure Test_Extract_Mins (T : in out Test_Type);

   package Caller is new AUnit.Test_Caller (Test_Type);

   Test_Extract_Mins_Access : Caller.Test_Method := Test_Extract_Mins'Access;
   Test_Inserts_Access      : Caller.Test_Method := Test_Inserts'Access;
   Test_Clear_Access        : Caller.Test_Method := Test_Clear'Access;
   Test_Is_Empty_Access     : Caller.Test_Method := Test_Is_Empty'Access;

end DB.Utils.Gen_Binary_Heaps.Gen_Test;

