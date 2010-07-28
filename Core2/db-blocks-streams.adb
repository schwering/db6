-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Conversion;

package body DB.Blocks.Streams is

   function New_Stream
     (Block_Ref  : not null Base_Block_Ref_Type;
      Cursor_Ref : Cursor_Ref_Type := null)
      return Stream_Type is
   begin
      if Cursor_Ref /= null then
         return Stream_Type'(Ada.Streams.Root_Stream_Type with
                             Block  => Block_Ref,
                             Pos    => Cursor_Ref.Pos,
                             Cursor => Cursor_Ref);
      else
         return Stream_Type'(Ada.Streams.Root_Stream_Type with
                             Block  => Block_Ref,
                             Pos    => Block_Ref'First,
                             Cursor => null);
      end if;
   end New_Stream;


   procedure Sync_Cursor (Stream : in out Stream_Type) is
   begin
      if Stream.Cursor /= null then
         Stream.Cursor.Pos := Stream.Pos;
      end if;
   end Sync_Cursor;


   overriding
   procedure Write
     (Stream : in out Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array)
   is
      subtype Constr_Stream_Elem_Array is
         Ada.Streams.Stream_Element_Array (Item'Range);
      subtype Constr_Block_Type is
         Base_Block_Type (1 .. Item'Length);
      function Convert is new Ada.Unchecked_Conversion
        (Constr_Stream_Elem_Array, Constr_Block_Type);
   begin
      Stream.Block (Stream.Pos .. Stream.Pos + Item'Length - 1) :=
         Convert (Item);
      Stream.Pos := Stream.Pos + Item'Length;
      Sync_Cursor (Stream);
   end Write;


   overriding
   procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset)
   is
      subtype Constr_Stream_Elem_Array is
         Ada.Streams.Stream_Element_Array (Item'Range);
      subtype Constr_Block_Type is
         Base_Block_Type (1 .. Item'Length);
      function Convert is new Ada.Unchecked_Conversion
        (Constr_Block_Type, Constr_Stream_Elem_Array);
   begin
      Item := Convert (Stream.Block (Stream.Pos ..
                                     Stream.Pos + Item'Length - 1));
      Last := Item'Last;
      Stream.Pos := Stream.Pos + Item'Length;
      Sync_Cursor (Stream);
   end Read;


   procedure Seek
     (Stream : in out Stream_Type;
      Pos    : in     Ada.Streams.Stream_Element_Offset) is
   begin
      Stream.Pos := Base_Position_Type (Pos);
   end Seek;


   procedure Seek
     (Stream : in out Stream_Type;
      Cursor : in     Cursor_Type) is
   begin
      Stream.Pos := Cursor.Pos;
      Sync_Cursor (Stream);
   end Seek;

end DB.Blocks.Streams;

