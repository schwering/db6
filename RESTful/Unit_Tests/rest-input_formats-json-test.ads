-- Abstract:
--
-- Unit test for JSON deserialization.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

private with Ada.Strings.Unbounded;

with AUnit.Test_Fixtures;

package REST.Input_Formats.JSON.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   overriding
   procedure Set_Up (T : in out Test_Type);
   overriding
   procedure Tear_Down (T : in out Test_Type);

   procedure Test_Flat (T : in out Test_Type);
   procedure Test_Nested (T : in out Test_Type);
   procedure Test_Array (T : in out Test_Type);
   procedure Test_Escape (T : in out Test_Type);

private
   use Ada.Strings.Unbounded;

   type Event_Type is
      (Anonymous_Object_Start,
       Object_Start,
       Object_End,
       Anonymous_Array_Start,
       Array_Start,
       Array_End,
       Value,
       Anonymous_Value,
       EOF,
       Error);

   type Item_Type (Event : Event_Type := Error) is
      record
         case Event is
            when Object_Start | Array_Start | Anonymous_Value | Value =>
               Key : Unbounded_String;
               case Event is
                  when Anonymous_Value | Value =>
                     Value : DB.Types.Values.Value_Type;
                  when others => null;
               end case;
            when others => null;
         end case;
      end record;

   type Item_Array_Type is array (Positive range <>) of Item_Type;

   type Parser_Ref_Type is access Parser_Type;

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with
      record
         P : Parser_Ref_Type := null;
      end record;

end REST.Input_Formats.JSON.Test;

