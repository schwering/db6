-- Abstract:
--
-- This implementation of Gen_Buffers is simple in the sense that it just
-- ignores the possibility of hardware crashes during commits.
--
-- Furthermore, the blocks are hold in a linked list, whereas a binary tree
-- would allow faster Read operations (this probably wouldn't bring much
-- performance improvement, because the buffers usuall are small).
--
-- Uses the dynamic memory allocation, of course.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Storage_Pools;

with DB.IO.Blocks.Gen_IO;
with DB.IO.Blocks.Gen_IO.Gen_Buffers;

generic
   with package Block_IO is new Gen_IO (<>);
   Node_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
package DB.IO.Blocks.Gen_Simple_Buffers is
   pragma Preelaborate;

   type Entry_Type;
   type Entry_Ref_Type is access Entry_Type;
   for Entry_Ref_Type'Storage_Pool use Node_Storage_Pool;

   type Entry_Type is
      record
         Next    : Entry_Ref_Type;
         Address : Block_IO.Valid_Address_Type;
         Block   : Block_Type;
         Changed : Boolean := False;
      end record;

   type Buffer_Type is
      record
         Head                    : Entry_Ref_Type := null;
         New_Address_Initialized : Boolean := False;
         New_Address             : Block_IO.Valid_Address_Type;
      end record;


   function New_Buffer
      return Buffer_Type;
   -- Creates a new buffer.

   procedure Free
     (Buffer : in out Buffer_Type);
   -- Frees all resources hold by the buffer.

   procedure Commit
     (File   : in out Block_IO.File_Type;
      Buffer : in     Buffer_Type);
   -- Flushes all dirty blocks in the buffer. See Write and Read.

   procedure Seek_New
     (File    : in out Block_IO.File_Type;
      Buffer  : in out Buffer_Type;
      Address :    out Block_IO.Valid_Address_Type);
   -- Sets Address to the value it would be set to by Block_IO.Seek_New if
   -- those blocks that are currently in the Buffer were written to file
   -- directly.

   procedure Read
     (File    : in out Block_IO.File_Type;
      Buffer  : in out Buffer_Type;
      Address : in     Block_IO.Valid_Address_Type;
      Block   :    out Block_Type);
   -- Reads an Block from the Buffer or from file if no Block corresponding 
   -- with Address is present in the Buffer. The read Block is not stored
   -- in the Buffer.

   procedure Write
     (File     : in out Block_IO.File_Type;
      Buffer   : in out Buffer_Type;
      Address  : in     Block_IO.Valid_Address_Type;
      Block    : in     Block_Type);
   -- Writes an Block to the Buffer, not to the file itself.


   package Buffers is new Block_IO.Gen_Buffers
     (Buffer_Type => Buffer_Type,
      New_Buffer  => New_Buffer,
      Free        => Free,
      Commit      => Commit,
      Seek_New    => Seek_New,
      Read        => Read,
      Write       => Write);


private
   function "<="
     (A, B : Block_IO.Valid_Address_Type)
      return Boolean;

end DB.IO.Blocks.Gen_Simple_Buffers;

