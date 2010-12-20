-- Abstract:
--
-- Integer and enumeration value implementation.
-- It uses the formal parameter's 'Read and 'Write attributes.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams;

generic
   type Integer_Type is (<>);
package DB.Maps.Values.Gen_Integers is

   type Value_Type is new Maps.Value_Type with private;

   overriding
   function New_Value
     (Params : not null access Value_Parameters_Type)
      return Value_Type;

   function New_Value (I : Integer_Type) return Value_Type;

   overriding
   function Size_Bound
      (Value : Value_Type)
       return Ada.Streams.Stream_Element_Offset;

   overriding
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : in              Value_Type);

   overriding
   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : out             Value_Type);

   overriding
   function Equals (A, B : Value_Type) return Boolean;

   overriding
   function Image (Value : Value_Type) return String;

   function Value (Value : Value_Type) return Integer_Type;

private
   type Value_Type is new Maps.Value_Type with
      record
         Int : Integer_Type;
      end record;

end DB.Maps.Values.Gen_Integers;

