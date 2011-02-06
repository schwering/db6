-- Abstract:
--
-- Tests some HTTP DELETE requests.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Fixtures;

package REST.Delete.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   overriding
   procedure Set_Up (T : in out Test_Type);
   overriding
   procedure Tear_Down (T : in out Test_Type);

   procedure Test_Range (T : in out Test_Type);
   procedure Test_Single (T : in out Test_Type);

private
   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with
      record
         null;
      end record;

end REST.Delete.Test;

