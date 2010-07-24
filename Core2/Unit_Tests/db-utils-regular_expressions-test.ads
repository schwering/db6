-- Abstract:
--
-- Regular expression test fixture.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Test_Fixtures;

package DB.Utils.Regular_Expressions.Test is

   type Regexp_Ref_Type is access Regexp_Type;
   type Regexp_Ref_Array_Type is array (Positive range <>) of Regexp_Ref_Type;

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with
      record
         A_Plus  : Regexp_Ref_Type;
         B_Plus  : Regexp_Ref_Type;
         C_Plus  : Regexp_Ref_Type;

         AB_Plus : Regexp_Ref_Type;
         BA_Plus : Regexp_Ref_Type;

         Regexps : Regexp_Ref_Array_Type (1 .. 5);
      end record;

   function Regexp (T : Test_Type; I : Natural) return Regexp_Type;

   overriding
   procedure Set_Up (T : in out Test_Type);

   overriding
   procedure Tear_Down (T : in out Test_Type);

   procedure Test_Empty_Regexp (T : in out Test_Type);
   procedure Test_Union (T : in out Test_Type);
   procedure Test_Intersection (T : in out Test_Type);
   procedure Test_Difference (T : in out Test_Type);

end DB.Utils.Regular_Expressions.Test;

