-- Abstract:
--
-- A simple stream implementation that wraps block.
-- Note that the stream must not live longer than the block that it wraps.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams;

package DB.Blocks.Streams is
   pragma Pure;

   type Base_Block_Ref_Type is access all Base_Block_Type;
   for Base_Block_Ref_Type'Storage_Size use 0;
   pragma Controlled (Base_Block_Ref_Type);

   type Stream_Type is new Ada.Streams.Root_Stream_Type with private;

   function New_Stream
     (Block_Ref : Base_Block_Ref_Type)
      return Stream_Type;

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


private
   type Stream_Type is new Ada.Streams.Root_Stream_Type with
      record
         Block  : Base_Block_Ref_Type;
         Pos    : Base_Position_Type;
      end record;

end DB.Blocks.Streams;

