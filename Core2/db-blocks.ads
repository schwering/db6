-- Abstract:
--
-- Disk block container.
--
-- Design Notes:
--
-- A crucial number is Block_Size. As pointed out below, Last_Position depends
-- on Block_Size, because a Base_Block_Type should provide some overflow space.
--
-- The native block size of disks is usually 4k (2**12 bytes). Since hard disks
-- spend most time seeking for addresses, bigger consecutive blocks are
-- interesting, too. An analysis is complicated by the fact that the seek time
-- is compensated by the IO cache for small files. Hence, the following tests
-- were carried out with direct IO (see the Low_Level_IO package and the
-- underlying C functions with the O_DIRECT flag).
-- The run test was a small set of append, delete and search operations of the
-- Gen_BTrees package (test3.sh).
--
--        Total   | Append   | Delete   | Search
-- ---------------+----------+---------+---------
-- 4k:   36m 38s  |  5m 15s  |  3m 35s  |  1m 41s
-- 32k:  25m 15s  |  3m 27s  |  2m  7s  |  1m 19s
-- 64k:  25m 15s  |  3m 27s  |  2m  7s  |  1m 19s
-- 128k: 18m 25s  |  3m  1s  |  2m 38s  |     45s
-- 256k: 18m 54s  |  3m 14s  |  2m 59s  |     45s
--
-- The advantage of 32k or less is that the size fits into 16-bit-integer;
-- bigger blocks require 24-bit-integers. This should explain why 64k is worse
-- than 32k.
-- The disadvantage of large blocks is the risk of stack overflows. The test
-- crashed for 512k and upwards.
-- An advantage of big blocks is probably better compression.
--
-- As a consequence of this test, 128k is currently chosen as block size.
-- For small files, when the IO cache prevents seek times from dominating the IO
-- access time, 128k is a good deal worse than 4k or so.
--
-- Copyright 2008--2011 Christoph Schwering

with System.Storage_Elements;

package DB.Blocks is
   pragma Pure;

   subtype Storage_Element_Type is System.Storage_Elements.Storage_Element;
   subtype Size_Type is System.Storage_Elements.Storage_Offset;
   use type Size_Type;

   Block_Size    : constant := 2**(7 + 10);     -- 128k
   Last_Position : constant := 2**(8 + 10) - 1; -- 256k - 1
   -- Block_Size is the count of bytes written to disk at once, whereas
   -- Last_Position is maximum position allowed in block-arrays. Last_Position
   -- must be greater than Block_Size in order to allow blocks which are
   -- truncated before actually being written.

   type Base_Position_Type is range 0 .. Last_Position;
   for Base_Position_Type'Size use 24;
   subtype Base_Index_Type is Base_Position_Type range 1 .. Last_Position;
   type Base_Block_Type is
      array (Base_Index_Type range <>) of aliased Storage_Element_Type;

   subtype Position_Type is Base_Position_Type range 0 .. Block_Size + 1;
   subtype Index_Type is Base_Index_Type range 1 .. Block_Size;
   subtype Block_Type is Base_Block_Type (Index_Type);

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

   function New_Cursor
     (Block : Base_Block_Type;
      Start : Base_Position_Type)
      return Cursor_Type;

   function Is_Valid (Cursor : Cursor_Type) return Boolean;
   -- Indicates whether or not the given Cursor is at a valid or not.
   -- Invalid positions denote that a written or read object did not fit into
   -- the cursor.
   -- Operations on invalid cursors might raise exceptions, hence the user
   -- should always validate.

   function Position (Cursor : Cursor_Type) return Base_Position_Type;
   -- Returns the current position of the cursor.

   function Remaining_Space (Cursor : Cursor_Type) return Size_Type;
   -- Returns the number of units that can be written to or read from Cursor.

   function Moved_Since
     (Cursor : Cursor_Type;
      Since  : Base_Position_Type)
      return Size_Type;
   -- Returns by how many units the cursor was moved since it was at position
   -- Since.

   function Bits_To_Units (Bits : Size_Type) return Size_Type;
   pragma Pure_Function (Bits_To_Units);
   -- Determines how many bytes are needed to store Bits bits.

   procedure Restrict
     (Cursor : in out Cursor_Type;
      Last   : in     Base_Position_Type);
   -- Restricts the last index of the cursor to Last.
   -- Validations of Cursor with Is_Valid should be done before calling
   -- Unrestrict.

   procedure Unrestrict
     (Cursor : in out Cursor_Type;
      Block  : in     Base_Block_Type);
   -- Resets any restrictions on Cursor's last index to Block'Last.
   -- Validations of Cursor with Is_Valid should be done before calling
   -- Unrestrict.

   procedure Reset (Block : in out Base_Block_Type);
   -- Sets all bytes of Block to zero.

   procedure Reset_Free_Space_Of_Block
     (Block     : in out Base_Block_Type;
      Last_Used : in     Base_Position_Type);
   -- Resets the bytes behind Last_Used until the Block_Size-th byte. Similarly
   -- to the To_Block functions, this is done to prepare the block for being
   -- written to disk.

   generic
      type Item_Type is private;
   function Size_Of (Item : Item_Type) return Size_Type;

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
   -- Writes Item to Block at the position denoted by Cursor.
   -- If Cursor is invalid, the behavior is undefined. Use Is_Valid.

   generic
      type Item_Type is private;
   procedure Read
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type;
      Item   :    out Item_Type);
   -- Reads Item from Block at the position denoted by Cursor.
   -- If Cursor is invalid, the behavior is undefined. Use Is_Valid.

   generic
      type Item_Type is private;
   procedure Skip
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type);
   -- Skips an item of Item_Type from Block at the position denoted by Cursor.
   -- If Cursor is invalid, the behavior is undefined. Use Is_Valid.

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
         Pos  : Base_Position_Type := Base_Index_Type'First;
         Last : Base_Position_Type := Base_Index_Type'Last;
         -- Pos = Cursor.Last + 1 means that the last write was successful,
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
   pragma Inline (Reset_Free_Space_Of_Block);
   pragma Inline (Size_Of);
   pragma Inline (Write);
   pragma Inline (Write_At);
   pragma Inline (Read);
   pragma Inline (Read_At);
   pragma Inline (Size_Of_Array);
   pragma Inline (Write_Array);
   pragma Inline (Read_Array);

end DB.Blocks;

