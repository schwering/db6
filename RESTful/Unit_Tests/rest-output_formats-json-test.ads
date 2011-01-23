-- Abstract:
--
-- JSON serialization test.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

private with Ada.Strings.Unbounded;

with AUnit.Test_Fixtures;

private with REST.Input_Formats;

package REST.Output_Formats.JSON.Test is

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with private;

   overriding
   procedure Set_Up (T : in out Test_Type);
   overriding
   procedure Tear_Down (T : in out Test_Type);

   procedure Test_Flat (T : in out Test_Type);
   procedure Test_Nested (T : in out Test_Type);
   procedure Test_Array (T : in out Test_Type);
   procedure Test_Escaped (T : in out Test_Type);

private
   use Ada.Strings.Unbounded;

   type Test_Type is new AUnit.Test_Fixtures.Test_Fixture with
      record
         null;
      end record;

   type Event_Type is
      (Anonymous_Object_Start,
       Object_Start,
       Object_End,
       Anonymous_Array_Start,
       Array_Start,
       Array_End,
       Value,
       Anonymous_Value,
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

   type Item_Array_Ref_Type is access all Item_Array_Type;

   type Handler_Type is new Input_Formats.Handler_Type with
      record
         Expected : Item_Array_Ref_Type;
         I        : Natural := 0;
      end record;

   overriding
   procedure Start_Anonymous_Object (Handler : in out Handler_Type);

   overriding
   procedure Start_Object
     (Handler : in out Handler_Type;
      Key     : in     String);

   overriding
   procedure End_Object (Handler : in out Handler_Type);

   overriding
   procedure Start_Anonymous_Array (Handler : in out Handler_Type);

   overriding
   procedure Start_Array
     (Handler : in out Handler_Type;
      Key     : in     String);

   overriding
   procedure End_Array (Handler : in out Handler_Type);

   overriding
   procedure Anonymous_Value
     (Handler : in out Handler_Type;
      Val     : in     DB.Types.Values.Value_Type);

   overriding
   procedure Value
     (Handler : in out Handler_Type;
      Key     : in     String;
      Val     : in     DB.Types.Values.Value_Type);

   overriding
   procedure Error (Handler : in out Handler_Type);

end REST.Output_Formats.JSON.Test;

