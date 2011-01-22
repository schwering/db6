-- Abstract:
--
-- Tests HTTP GET.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Fixtures;

package REST.Get.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   overriding
   procedure Set_Up (T : in out Test_Type);
   overriding
   procedure Tear_Down (T : in out Test_Type);

   procedure Test_Excl_Excl (T : in out Test_Type);
   procedure Test_Excl_Incl (T : in out Test_Type);
   procedure Test_Incl_Incl (T : in out Test_Type);
   procedure Test_Incl_Excl (T : in out Test_Type);

   procedure Test_Empty_Excl_Excl (T : in out Test_Type);
   procedure Test_Empty_Excl_Incl (T : in out Test_Type);
   procedure Test_Empty_Incl_Incl (T : in out Test_Type);
   procedure Test_Empty_Incl_Excl (T : in out Test_Type);

   procedure Test_Infinity (T : in out Test_Type);
   procedure Test_Count (T : in out Test_Type);

private
   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with
      record
         null;
      end record;

end REST.Get.Test;

