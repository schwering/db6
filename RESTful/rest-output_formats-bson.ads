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

   type Writer_Type is new Output_Formats.Writer_Type with private;

   overriding
   function Content_Type (Writer : Writer_Type) return String;

   overriding
   procedure Start_Anonymous_Object (Resource : in out Writer_Type);

   overriding
   procedure Start_Object
     (Resource : in out Writer_Type;
      Key      : in     String);

   overriding
   procedure End_Object (Resource : in out Writer_Type);

   overriding
   procedure Start_Anonymous_Array (Resource : in out Writer_Type);

   overriding
   procedure Start_Array
     (Resource : in out Writer_Type;
      Key      : in     String);

   overriding
   procedure End_Array (Resource : in out Writer_Type);

   overriding
   procedure Put_Value
     (Resource : in out Writer_Type;
      Key      : in     String;
      Value    : in     DB.Maps.Value_Type'Class);

   overriding
   procedure Put_Anonymous_Value
     (Resource : in out Writer_Type;
      Value    : in     DB.Maps.Value_Type'Class);

private
   subtype Byte_Type is AS.Stream_Element;
   subtype Byte_Array_Type is AS.Stream_Element_Array;

   type Writer_Type is new Output_Formats.Writer_Type with null record;

end REST.Output_Formats.BSON;

