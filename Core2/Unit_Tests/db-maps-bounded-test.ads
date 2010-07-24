-- Abstract:
--
-- Regular expression test fixture.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Fixtures;

package DB.Maps.Bounded.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   overriding
   procedure Set_Up (T : in out Test_Type);

   overriding
   procedure Tear_Down (T : in out Test_Type);

private
   type Map_Ref_Type is access Map_Type;
   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with
      record
         Map : Map_Ref_Type;
      end record;

end DB.Maps.Bounded.Test;

