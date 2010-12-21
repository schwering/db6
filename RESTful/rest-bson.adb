-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Conversion;

package body REST.BSON is

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


   procedure Emit
     (Resource : in out Stream_Type;
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


   procedure Start_BSON
     (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "{");
   end Start_BSON;


   procedure End_BSON
     (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "}");
      Queues.Mark_Final (Resource.Queue);
   end End_BSON;


   procedure Put_Float
     (Resource : in out Stream_Type;
      Key      : in     String;
      Float    : in     Float_Value_Type) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, Float.Image);
      Emit (Resource, ",");
   end Put_Float;


   procedure Put_Integer
     (Resource : in out Stream_Type;
      Key      : in     String;
      Integer  : in     Integer_Value_Type) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, Integer.Image);
      Emit (Resource, ",");
   end Put_Integer;


   procedure Put_String
     (Resource : in out Stream_Type;
      Key      : in     String;
      Str      : in     String_Value_Type) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=""");
      Emit (Resource, Str.Image);
      Emit (Resource, """,");
   end Put_String;


   procedure Put_Null
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, "null");
      Emit (Resource, ",");
   end Put_Null;


   procedure Put_Object
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, "{");
   end Put_Object;


   procedure End_Object (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "},");
   end End_Object;


   procedure Put_Array
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, "[");
   end Put_Array;


   procedure End_Array (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "],");
   end End_Array;


   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out AS.Stream_Element_Array;
      Last     :    out AS.Stream_Element_Offset)
   is
      pragma Assert (Buffer'Component_Size =
                     Queues.Item_Array_Type'Component_Size); 
      use type AS.Stream_Element_Offset;
      Q_Buf : Queues.Item_Array_Type
        (Natural (Buffer'First) .. Natural (Buffer'Last));
      for Q_Buf'Address use Buffer'Address;
      Q_Last : Natural;
   begin
      pragma Assert (Buffer'Size = Q_Buf'Size);
      if Buffer'Length = 0 then
         Last := Buffer'First - 1;
         return;
      end if;

      Queues.Dequeue (Resource.Queue, Q_Buf, Q_Last);
      Last := AS.Stream_Element_Offset (Q_Last);
   end Read;


   procedure Close (Resource : in out Stream_Type) is
   begin
      Queues.Mark_Final (Resource.Queue);
   end Close;


   function End_Of_File (Resource : Stream_Type) return Boolean is
   begin
      return Queues.Is_Final (Resource.Queue);
   end End_Of_File;

end REST.BSON;

