-- Abstract:
--
-- Smart pointer tests.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Fixtures;

package DB.Utils.Smart_Pointers.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   procedure Test_Memory_Management (T : in out Test_Type);

private
   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with null record;

end DB.Utils.Smart_Pointers.Test;

