-- Abstract:
--
-- Tag map test fixture.
--
-- Copyright 2008--2011 Christoph Schwering

with AUnit.Test_Fixtures;

with DB.Maps.Tag_Map.Utils;

package DB.Maps.Tag_Map.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   overriding
   procedure Set_Up (T : in out Test_Type);

   overriding
   procedure Tear_Down (T : in out Test_Type);

   procedure Tests (T : in out Test_Type);

private
   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with
      record
         State : Utils.Tag_Map_State_Type;
      end record;

end DB.Maps.Tag_Map.Test;

