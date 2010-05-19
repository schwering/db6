-- Abstract:
--
-- Disk block container.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Storage_Elements;

package DB.Blocks is
   pragma Pure;

   subtype Storage_Element_Type is System.Storage_Elements.Storage_Element;
   subtype Size_Type is System.Storage_Elements.Storage_Offset;
   use type Size_Type;

   Block_Size : constant := 4 * 1024;

   Last_Position : constant := 2**14 - 1;
   type Base_Position_Type is range 0 .. Last_Position;
   for Base_Position_Type'Size use 15;
   -- Base_Position_Type is intended to have some overflow space, for example
   -- for applications that use Base_Block_Type greater than disk Block_Types
   -- that are truncated to Block_Types before being written to disk. It's
   -- limited to 15 bits because this allows Base_Block_Types of 16383 bytes,
   -- hence almost 4 times the size of a normal Block_Size. However, there is
   -- still one bit free if someone wants to store an additional boolean in a
   -- packed record together with a Base_Position_Type.

   subtype Position_Type is Base_Position_Type range 0 .. Block_Size + 1;

   subtype Base_Index_Type is Base_Position_Type range 1 .. Last_Position;

   subtype Index_Type is Base_Index_Type range 1 .. Block_Size;

   type Base_Block_Type is
      array (Base_Index_Type range <>) of aliased Storage_Element_Type;
   subtype Block_Type is Base_Block_Type(Index_Type);

   type Cursor_Type (<>) is limited private;


   function To_Block
     (Block  : Base_Block_Type;
      Cursor : Cursor_Type)
      return Block_Type;
   -- Truncates the base block to a short one and sets the free storage behind
   -- the cursor. The latter is done because the block will probably be written
   -- to disk.

   function To_Block
     (Block     : Base_Block_Type;
      Last_Used : Base_Position_Type)
      return Block_Type;
   -- Truncates the base block to a short one and sets the remaining free
   -- storage elements to zero. The latter is done because the block will
   -- probably be written to disk.

   procedure Reset_Free_Space_Of_Block
     (Block     : in out Base_Block_Type;
      Last_Used : in     Base_Position_Type);
   -- Resets the bytes behind Last_Used until the Block_Size-th byte. Similarly
   -- to the To_Block functions, this is done to prepare the block for being
   -- written to disk.

   function New_Cursor
     (Start : Base_Position_Type)
      return Cursor_Type;

   function Is_Valid
     (Cursor : Cursor_Type)
      return Boolean;

   function Is_Valid
     (Position : Base_Position_Type)
      return Boolean;

   function Position
     (Cursor : Cursor_Type)
      return Base_Position_Type;

   function Remaining_Space
     (Block  : Base_Block_Type;
      Cursor : Cursor_Type)
      return Size_Type;

   function Moved_Since
     (Cursor : Cursor_Type;
      Since  : Base_Position_Type)
      return Size_Type;

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
      type Item_Type is private;
   procedure Skip
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type);

   generic
      type Index_Type is (<>);
      type Item_Type is private;
      type Array_Type is array (Index_Type) of Item_Type;
   function Size_Of_Array
     (Arr  : Array_Type;
      From : Index_Type;
      To   : Index_Type'Base)
      return Size_Type;

   generic
      type Index_Type is (<>);
      type Item_Type is private;
      type Array_Type is array (Index_Type) of Item_Type;
   procedure Write_Array
     (Block  : in out Base_Block_Type;
      Cursor : in out Cursor_Type;
      Arr    : in     Array_Type;
      From   : in     Index_Type;
      To     : in     Index_Type'Base);

   generic
      type Index_Type is (<>);
      type Item_Type is private;
      type Array_Type is array (Index_Type) of Item_Type;
   procedure Read_Array
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type;
      Arr    : in out Array_Type;
      From   : in     Index_Type;
      To     :    out Index_Type'Base);

   generic
      type Index_Type is (<>);
      type Item_Type is private;
   procedure Skip_Array
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type;
      From   : in     Index_Type);


private
   Invalid_Position : constant Base_Position_Type := 0;

   type Cursor_Type is
      record
         Pos : Base_Position_Type := Base_Index_Type'First;
         -- Pos = Block'Last + 1 means that the last write was successful,
         -- but no more data can be written, while
         -- Pos = 0 means that the Cursor has become invalid, which can
         -- be the case due to a write of data that does not fit in the
         -- remaining space of the Block.
      end record;

   pragma Inline (To_Block);
   pragma Inline (New_Cursor);
   pragma Inline (Is_Valid);
   pragma Inline (Position);
   pragma Inline (Moved_Since);
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

end DB.Blocks;

