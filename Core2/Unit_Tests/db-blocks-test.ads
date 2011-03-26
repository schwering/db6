-- Abstract:
--
-- Test cases for blocks.
--
-- Copyright 2008--2011 Christoph Schwering

with AUnit.Test_Fixtures;

package DB.Blocks.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   overriding
   procedure Set_Up (T : in out Test_Type);
   overriding
   procedure Tear_Down (T : in out Test_Type);

   procedure Test_Cursor (T : in out Test_Type);
   procedure Test_Utils (T : in out Test_Type);

private
   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with
      record
         null;
      end record;

end DB.Blocks.Test;

