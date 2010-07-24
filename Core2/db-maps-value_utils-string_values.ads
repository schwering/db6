-- Abstract:
--
-- String value implementation.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams;
with Ada.Finalization;

package DB.Maps.Value_Utils.String_Values is

   type Value_Type is new Maps.Value_Type with private;

   function Make (S : String) return Value_Type;

   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : in              Value_Type);

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : out             Value_Type);

   function Equals (A, B : Value_Type) return Boolean;

   function Image (Value : Value_Type) return String;


private
   package AF renames Ada.Finalization;

   type String_Ref_Type is access String;

   type Value_Type is new AF.Controlled and Maps.Value_Type with
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

