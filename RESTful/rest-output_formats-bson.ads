-- Abstract:
--
-- Serialization of database entries to BSON format.
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Streams;

with AWS.Resources.Streams;

package REST.Output_Formats.BSON is
   pragma Elaborate_Body;

   package AS renames Ada.Streams;
   package ARS renames AWS.Resources.Streams;

   type Stream_Type is new Output_Formats.Stream_Type with private;

   overriding
   function Content_Type (Stream : Stream_Type) return String;

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

private
   subtype Byte_Type is AS.Stream_Element;
   subtype Byte_Array_Type is AS.Stream_Element_Array;

   type Stream_Type is new Output_Formats.Stream_Type with null record;

end REST.Output_Formats.BSON;

