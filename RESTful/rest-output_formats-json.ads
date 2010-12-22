-- Abstract:
--
-- Serialization of database entries to JSON format.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams;

with AWS.Resources.Streams;

with DB.Maps;

package REST.Output_Formats.JSON is
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
   type Stream_Type is new Output_Formats.Stream_Type with
      record
         Indent : Natural := 0;
      end record;

end REST.Output_Formats.JSON;

