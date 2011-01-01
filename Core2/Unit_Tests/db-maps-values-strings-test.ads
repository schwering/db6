-- Abstract:
--
-- Tag map test fixture.
--
-- Copyright 2008--2011 Christoph Schwering

with AUnit.Test_Fixtures;

package DB.Maps.Values.Strings.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   procedure Test_Write_Read (T : in out Test_Type);
   procedure Test_Size_Bound (T : in out Test_Type);

private
   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with null record;

end DB.Maps.Values.Strings.Test;

