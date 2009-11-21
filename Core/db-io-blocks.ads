-- Abstract:
--
-- Disk block container.
--
-- Copyright 2008, 2009 Christoph Schwering

with System.Storage_Elements;

package DB.IO.Blocks is
   pragma Pure;

   subtype Storage_Element_Type is System.Storage_Elements.Storage_Element;
   subtype Size_Type is System.Storage_Elements.Storage_Offset;
   use type Size_Type;

   Block_Size      : constant := 4 * 1024;
   Long_Block_Size : constant := Block_Size * 5 / 4;

   type    Base_Position_Type is range 0 .. Long_Block_Size;
   for Base_Position_Type'Size use 16;
   subtype Position_Type      is Base_Position_Type range 0 .. Block_Size;
   subtype Long_Position_Type is Base_Position_Type;

   subtype Base_Index_Type is Base_Position_Type range 1 .. Long_Block_Size;
   subtype Index_Type      is Base_Index_Type range 1 .. Block_Size;
   subtype Long_Index_Type is Base_Index_Type;

   type Base_Block_Type is
      array (Base_Index_Type range <>) of Storage_Element_Type;
   subtype Block_Type      is Base_Block_Type(Index_Type);
   subtype Long_Block_Type is Base_Block_Type(Long_Index_Type);

   type Cursor_Type is limited private;


   function To_Block
     (Block     : Long_Block_Type;
      Last_Used : Long_Position_Type)
      return Block_Type;
   -- Truncates the long block to a short one and sets the remaining free
   -- storage elements to zero. The latter is done because the block will
   -- probably be written to disk.

   function To_Long_Block
     (Block : Block_Type)
      return Long_Block_Type;

   function New_Cursor
     (Start : Base_Index_Type)
      return Cursor_Type;

   function Is_Valid
     (Block  : Base_Block_Type;
      Cursor : Cursor_Type)
      return Boolean;

   function Position
     (Cursor : Cursor_Type)
      return Base_Position_Type;

   function Bits_To_Units
     (Bits : Size_Type)
      return Size_Type;

   procedure Reset
     (Block : in out Base_Block_Type);

   generic
      type Item_Type is private;
   function Size_Of
     (Item : Item_Type)
      return Size_Type;

   generic
      type Item_Type is private;
   procedure Write_At
     (Block      : in out Base_Block_Type;
      From_Index : in     Base_Index_Type;
      To_Index   : in     Base_Position_Type;
      Item       : in     Item_Type);

   generic
      type Item_Type is private;
   procedure Read_At
     (Block      : in  Base_Block_Type;
      From_Index : in  Base_Index_Type;
      To_Index   : in  Base_Position_Type;
      Item       : out Item_Type);

   generic
      type Item_Type is private;
   procedure Write
     (Block  : in out Base_Block_Type;
      Cursor : in out Cursor_Type;
      Item   : in     Item_Type);

   generic
      type Item_Type is private;
   procedure Read
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type;
      Item   :    out Item_Type);

   generic
      type Index_Type is (<>);
      type Item_Type is private;
      type Array_Type is array (Index_Type) of Item_Type;
   function Size_Of_Array
     (Arr : Array_Type;
      To  : Index_Type'Base := Array_Type'Last)
      return Size_Type;

   generic
      type Index_Type is (<>);
      type Item_Type is private;
      type Array_Type is array (Index_Type) of Item_Type;
   procedure Write_Array
     (Block  : in out Base_Block_Type;
      Cursor : in out Cursor_Type;
      Arr    : in     Array_Type;
      To     : in     Index_Type'Base := Array_Type'Last);

   generic
      type Index_Type is (<>);
      type Item_Type is private;
      type Array_Type is array (Index_Type) of Item_Type;
   procedure Read_Array
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type;
      Arr    :    out Array_Type;
      To     :    out Index_Type'Base);


private
   type Cursor_Type is limited
      record
         Pos : Base_Position_Type := Base_Index_Type'First;
      end record;

   pragma Inline (To_Block);
   pragma Inline (To_Long_Block);
   pragma Inline (New_Cursor);
   pragma Inline (Is_Valid);
   pragma Inline (Position);
   pragma Inline (Bits_To_Units);
   pragma Inline (Reset);
   pragma Inline (Size_Of);
   pragma Inline (Write);
   pragma Inline (Write_At);
   pragma Inline (Read);
   pragma Inline (Read_At);
   pragma Inline (Size_Of_Array);
   pragma Inline (Write_Array);
   pragma Inline (Read_Array);

end DB.IO.Blocks;

