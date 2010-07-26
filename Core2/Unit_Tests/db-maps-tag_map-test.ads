-- Abstract:
--
-- Tag map test fixture.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Fixtures;

package DB.Maps.Tag_Map.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   procedure Test_Register (T : in out Test_Type);
   procedure Test_Seal (T : in out Test_Type);
   procedure Test_Map (T : in out Test_Type);

private
   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with null record;

end DB.Maps.Tag_Map.Test;

