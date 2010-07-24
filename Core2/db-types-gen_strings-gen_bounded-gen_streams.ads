-- Abstract:
--
-- A simple stream implementation that wraps a string.
-- Note that the stream must not live longer than the string that it writes to
-- and reads from.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams;

generic
package DB.Types.Gen_Strings.Gen_Bounded.Gen_Streams is
   pragma Pure;

   type String_Ref_Type is access all String_Type;
   for String_Ref_Type'Storage_Size use 0;
   pragma Controlled (String_Ref_Type);

   type Stream_Type is new Ada.Streams.Root_Stream_Type with private;

   function New_Stream (String_Ref : String_Ref_Type) return Stream_Type;

   overriding
   procedure Write
     (Stream : in out Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array);

   overriding
   procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset);

   procedure Seek
     (Stream : in out Stream_Type;
      Pos    : in     Ada.Streams.Stream_Element_Offset);

   function Remaining (Stream : Stream_Type) return Length_Type;


private
   type Stream_Type is new Ada.Streams.Root_Stream_Type with
      record
         String : String_Ref_Type;
         Pos    : Length_Type;
      end record;

end DB.Types.Gen_Strings.Gen_Bounded.Gen_Streams;

