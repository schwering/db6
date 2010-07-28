-- Abstract:
--
-- String value implementation.
-- It's kind of inefficient because it uses String'Input/Output which includes
-- the string length. This information is also accessible via
-- Types.Values.Bounded.Streams.Remaining. Besides that, it would suffice to
-- use a terminating null-byte.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams;
with Ada.Finalization;

package DB.Maps.Value_Utils.String_Values is

   type Value_Type is new Maps.Value_Type with private;

   overriding
   function New_Value
     (Params : not null access Value_Parameters_Type)
      return Value_Type;

   function New_Value (S : String) return Value_Type;

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
   package AF renames Ada.Finalization;

   type String_Ref_Type is access String;

   type Value_Type is new Maps.Value_Type with
      record
         Str : String_Ref_Type;
      end record;

   overriding
   procedure Initialize (Value : in out Value_Type);

   overriding
   procedure Adjust (Value : in out Value_Type);

   overriding
   procedure Finalize (Value : in out Value_Type);

end DB.Maps.Value_Utils.String_Values;

