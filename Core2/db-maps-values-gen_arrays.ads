-- Abstract:
--
-- Array value implementation.
-- It uses the formal parameter's 'Read and 'Write attributes.
-- The length is stored as 2 byte integer.
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Streams;

generic
   type Item_Type is (<>);
   type Array_Type is array (Positive range <>) of Item_Type;
package DB.Maps.Values.Gen_Arrays is

   type Value_Type is new Maps.Value_Type with private;

   overriding
   function New_Value
     (Params : not null access Value_Parameters_Type)
      return Value_Type;

   function New_Value (S : Array_Type) return Value_Type;

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

   type Array_Ref_Type is access Array_Type;

   type Value_Type is new Maps.Value_Type with
      record
         Str : Array_Ref_Type;
      end record;

   overriding
   procedure Initialize (Value : in out Value_Type);

   overriding
   procedure Adjust (Value : in out Value_Type);

   overriding
   procedure Finalize (Value : in out Value_Type);

end DB.Maps.Values.Gen_Arrays;

