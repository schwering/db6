-- Abstract:
--
-- Tag map test fixture.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Fixtures;

package DB.Maps.Value_Utils.String_Values.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   procedure Test_Write_Read (T : in out Test_Type);
   procedure Test_Size_Bound (T : in out Test_Type);

private
   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with null record;

end DB.Maps.Value_Utils.String_Values.Test;

