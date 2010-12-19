-- Abstract:
--
-- Integer value implementation.
-- It uses Integer'Read/Write.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams;

package DB.Maps.Values.Integers is
   pragma Elaborate_Body;

   type Value_Type is new Maps.Value_Type with private;

   overriding
   function New_Value
     (Params : not null access Value_Parameters_Type)
      return Value_Type;

   function New_Value (I : Integer) return Value_Type;

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


private
   type Value_Type is new Maps.Value_Type with
      record
         Int : Integer;
      end record;

end DB.Maps.Values.Integers;

