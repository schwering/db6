-- Abstract:
--
-- Serialization of database entries to JSON format.
--
-- Copyright 2008--2011 Christoph Schwering

package REST.Input_Formats.JSON is

   type Stream_Type is new Input_Formats.Stream_Type with private;

   overriding
   procedure Start_Anonymous_Object (Resource : in out Stream_Type);

   overriding
   procedure Start_Object
     (Resource : in out Stream_Type;
      Key      : in     String);

   overriding
   procedure End_Object (Resource : in out Stream_Type);

   overriding
   procedure Start_Anonymous_Array (Resource : in out Stream_Type);

   overriding
   procedure Start_Array
     (Resource : in out Stream_Type;
      Key      : in     String);

   overriding
   procedure End_Array (Resource : in out Stream_Type);

   overriding
   procedure Put_Value
     (Resource : in out Stream_Type;
      Key      : in     String;
      Value    : in     DB.Maps.Value_Type'Class);

   overriding
   procedure Write
     (Resource : in out Stream_Type;
      Buffer   : in     AWS.Status.Stream_Element_Array;
      Last     : in     AWS.Status.Stream_Element_Offset);

private
   type Stream_Type is new Input_Formats.Stream_Type with null record;

end REST.Input_Formats.JSON;

