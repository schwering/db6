-- Abstract:
--
-- Serialization of database entries to JSON format.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams;

with AWS.Resources.Streams;

private with DB.DSA.Utils.Gen_Queues;
with DB.Maps.Values.Long_Floats;
with DB.Maps.Values.Long_Integers;
with DB.Maps.Values.Strings;

package REST.JSON is
   pragma Elaborate_Body;

   package AS renames Ada.Streams;
   package ARS renames AWS.Resources.Streams;

   type Stream_Type is new ARS.Stream_Type with private;

   ----------
   -- Operations to manipulate the content of the stream.

   subtype Float_Value_Type is DB.Maps.Values.Long_Floats.Value_Type'Class;
   subtype Integer_Value_Type is DB.Maps.Values.Long_Integers.Value_Type'Class;
   subtype String_Value_Type is DB.Maps.Values.Strings.Value_Type'Class;

   procedure Start_JSON
     (Resource : in out Stream_Type);

   procedure End_JSON
     (Resource : in out Stream_Type);

   procedure Put_Float
     (Resource : in out Stream_Type;
      Key      : in     String;
      Float    : in     Float_Value_Type);

   procedure Put_Integer
     (Resource : in out Stream_Type;
      Key      : in     String;
      Integer  : in     Integer_Value_Type);

   procedure Put_String
     (Resource : in out Stream_Type;
      Key      : in     String;
      Str      : in     String_Value_Type);

   procedure Put_Null
     (Resource : in out Stream_Type;
      Key      : in     String);

   procedure Put_Object
     (Resource : in out Stream_Type;
      Key      : in     String);

   procedure Put_Array
     (Resource : in out Stream_Type;
      Key      : in     String);

   procedure End_Object (Resource : in out Stream_Type);

   procedure End_Array (Resource : in out Stream_Type);

   ----------
   -- Operations to read the stream.

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out AS.Stream_Element_Array;
      Last     :    out AS.Stream_Element_Offset);

   procedure Close (Resource : in out Stream_Type);

   function End_Of_File (Resource : Stream_Type) return Boolean;

   procedure Reset (Resource : in out Stream_Type) is null;

   procedure Set_Index
     (Resource : in out Stream_Type;
      To       : in     AS.Stream_Element_Offset) is null;

private
   package Queues is new DB.DSA.Utils.Gen_Queues
     (Queue_Size => 1024,
      Item_Type  => AS.Stream_Element);

   type Stream_Type is new ARS.Stream_Type with
      record
         Queue : Queues.Queue_Type;
      end record;

end REST.JSON;

