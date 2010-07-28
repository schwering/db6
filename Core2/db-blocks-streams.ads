-- Abstract:
--
-- A simple stream implementation that wraps a block.
-- Note that the stream must not live longer than the block that it writes to
-- and reads from.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams;

package DB.Blocks.Streams is
   pragma Pure;

   type Base_Block_Ref_Type is access all Base_Block_Type;
   for Base_Block_Ref_Type'Storage_Size use 0;
   pragma Controlled (Base_Block_Ref_Type);

   type Cursor_Ref_Type is access all Cursor_Type;
   for Cursor_Ref_Type'Storage_Size use 0;
   pragma Controlled (Cursor_Ref_Type);

   type Stream_Type is new Ada.Streams.Root_Stream_Type with private;

   function New_Stream
     (Block_Ref  : not null Base_Block_Ref_Type;
      Cursor_Ref : Cursor_Ref_Type := null)
      return Stream_Type;
   -- Creates a new stream that writes to Block_Ref. This implies that the
   -- stream may not live longer (more exactly: may not be used longer) than the
   -- block lives.
   -- If Cursor_Ref is not null, then the cursor is kept in sync with the
   -- stream.

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

   procedure Seek
     (Stream : in out Stream_Type;
      Cursor : in     Cursor_Type);

private
   type Stream_Type is new Ada.Streams.Root_Stream_Type with
      record
         Block  : Base_Block_Ref_Type;
         Pos    : Base_Position_Type;
         Cursor : Cursor_Ref_Type;
      end record;

end DB.Blocks.Streams;

