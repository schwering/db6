-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Unchecked_Conversion;

package body REST.Output_Formats.BSON is

   type BSON_Int32_Type is mod 2**32;
   for BSON_Int32_Type'Size use 32;
   subtype Four_Byte_Array_Type is Byte_Array_Type (1 .. 4);
   function Int32_To_Bytes is new Ada.Unchecked_Conversion
     (BSON_Int32_Type, Four_Byte_Array_Type);


   type BSON_Int64_Type is mod 2**64;
   for BSON_Int64_Type'Size use 64;
   subtype Eight_Byte_Array_Type is Byte_Array_Type (1 .. 8);
   function Int64_To_Bytes is new Ada.Unchecked_Conversion
     (BSON_Int64_Type, Eight_Byte_Array_Type);


   type BSON_Boolean_Type is mod 2**1;
   for BSON_Boolean_Type'Size use 8;
   function Boolean_To_Bytes is new Ada.Unchecked_Conversion
     (BSON_Boolean_Type, Byte_Type);


   type BSON_Control_Type is
     (BSON_Float,
      BSON_String,
      BSON_Object,
      BSON_Array,
      BSON_Binary,
      BSON_Undefined,
      BSON_Object_Id,
      BSON_Boolean,
      BSON_Date,
      BSON_Null,
      BSON_Regexp,
      BSON_DB_Pointer,
      BSON_Java_Script,
      BSON_Symbol,
      BSON_Java_Script_Unscoped,
      BSON_32_Integer,
      BSON_Timestamp,
      BSON_64_Integer,
      BSON_Max_Key,
      BSON_Min_Key);
   for BSON_Control_Type use
     (BSON_Float                => 16#01#,
      BSON_String               => 16#02#,
      BSON_Object               => 16#03#,
      BSON_Array                => 16#04#,
      BSON_Binary               => 16#05#,
      BSON_Undefined            => 16#06#,
      BSON_Object_Id            => 16#07#,
      BSON_Boolean              => 16#08#,
      BSON_Date                 => 16#09#,
      BSON_Null                 => 16#0A#,
      BSON_Regexp               => 16#0B#,
      BSON_DB_Pointer           => 16#0C#,
      BSON_Java_Script          => 16#0D#,
      BSON_Symbol               => 16#0E#,
      BSON_Java_Script_Unscoped => 16#0F#,
      BSON_32_Integer           => 16#10#,
      BSON_Timestamp            => 16#11#,
      BSON_64_Integer           => 16#12#,
      BSON_Max_Key              => 16#7F#,
      BSON_Min_Key              => 16#FF#);
   for BSON_Control_Type'Size use 8;


   -- A problem for us is that the size of an BSON-document (which is either a
   -- JSON object or a JSON array) must be known in advance to write the
   -- respective 32bit length integer.
   -- http://bsonspec.org/#/specification


   function Content_Type (Writer : Writer_Type) return String
   is
      pragma Unreferenced (Writer);
   begin
      return "application/content";
   end Content_Type;


   procedure Emit
     (Resource : in out Writer_Type;
      Str      : in     String)
   is
      pragma Assert (String'Component_Size =
                     Queues.Item_Array_Type'Component_Size); 
      Buf : Queues.Item_Array_Type (Str'Range);
      for Buf'Address use Str'Address;
      Last : Natural;
   begin
      pragma Assert (Str'Size = Buf'Size);
      loop
         Queues.Enqueue (Resource.Queue, Buf, Last);
         exit when Last = Buf'Last;
      end loop;
   end Emit;


   procedure Start_Anonymous_Object (Resource : in out Writer_Type) is
   begin
      Emit (Resource, "{");
   end Start_Anonymous_Object;


   procedure Start_Object
     (Resource : in out Writer_Type;
      Key      : in     String) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """ : ");
      Emit (Resource, "{");
   end Start_Object;


   procedure End_Object (Resource : in out Writer_Type) is
   begin
      Emit (Resource, "},");
   end End_Object;


   procedure Start_Anonymous_Array (Resource : in out Writer_Type) is
   begin
      Emit (Resource, "[");
   end Start_Anonymous_Array;


   procedure Start_Array
     (Resource : in out Writer_Type;
      Key      : in     String) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """ : ");
      Emit (Resource, "[");
   end Start_Array;


   procedure End_Array (Resource : in out Writer_Type) is
   begin
      Emit (Resource, "],");
   end End_Array;


   procedure Put_Value
     (Resource : in out Writer_Type;
      Key      : in     String;
      Value    : in     DB.Maps.Value_Type) is
   begin
      null;
   end Put_Value;


   procedure Put_Anonymous_Value
     (Resource : in out Writer_Type;
      Value    : in     DB.Maps.Value_Type) is
   begin
      null;
   end Put_Anonymous_Value;

end REST.Output_Formats.BSON;

