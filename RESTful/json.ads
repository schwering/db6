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

package JSON is
   pragma Elaborate_Body;

   package AS renames Ada.Streams;
   package ARS renames AWS.Resources.Streams;

   type Stream_Type is new ARS.Stream_Type with private;

   ----------
   -- Operations to manipulate the content of the stream.

   subtype Float_Value_Type is DB.Maps.Values.Long_Floats.Value_Type'Class;
   subtype Integer_Value_Type is DB.Maps.Values.Long_Integers.Value_Type'Class;
   subtype String_Value_Type is DB.Maps.Values.Strings.Value_Type'Class;

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
   type Marker_Type is (JSON_Array_End, JSON_Object_End);

   type JSON_Type is (JSON_Number, JSON_String, JSON_Boolean, JSON_Null,
                      JSON_Object, JSON_Array);

   type String_Ref_Type is access String;

   type Number_Type (Is_Real : Boolean := False) is
      record
         case Is_Real is
            when False => Int  : Long_Integer;
            when True  => Real : Long_Float;
         end case;
      end record;

   type Value_Type (Kind : JSON_Type := JSON_Null) is
      record
         case Kind is
            when JSON_Boolean => B : Boolean;
            when JSON_Number  => N : Number_Type;
            when JSON_String  => S : String_Ref_Type;
            when JSON_Null    => null;
            when JSON_Object  => null;
            when JSON_Array   => null;
         end case;
      end record;

   type Key_Value_Type is
      record
         Key   : String_Ref_Type;
         Value : Value_Type;
      end record;

   type Item_Kind_Type is (Key_Value, Marker);

   type Item_Type (Is_Key_Value : Boolean := True) is
      record
         case Is_Key_Value is
            when True  => Key_Value : Key_Value_Type;
            when False => Marker    : Marker_Type;
         end case;
      end record;

   package Queues is new DB.DSA.Utils.Gen_Queues
     (Queue_Size => 100,
      Item_Type  => Item_Type);

   type Stream_Type is new ARS.Stream_Type with
      record
         Queue  : Queues.Queue_Type;
         Buffer : String_Ref_Type := null;
         Init   : Boolean := False;
         Final  : Boolean := False;
      end record;

end JSON;

