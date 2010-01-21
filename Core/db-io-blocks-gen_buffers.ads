-- Abstract:
--
-- A non-persistent layer for IO operations that can be materialized.
-- This is used by writing transactions: they work on a buffer which is
-- materialized on a commit and simply forgotten on an abort.
--
-- Uses the dynamic memory allocation, of course.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Storage_Pools;

with DB.IO.Blocks.Gen_IO;

generic
   with package Block_IO is new Gen_IO (<>);
   Block_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   Node_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
package DB.IO.Blocks.Gen_Buffers is
   pragma Preelaborate;

   type Buffer_Type is private;

   type Block_Ref_Type is access Block_Type;
   for Block_Ref_Type'Storage_Pool use Block_Storage_Pool;
   pragma Controlled (Block_Ref_Type);

   type Block_Constant_Ref_Type is access constant Block_Type;
   for Block_Constant_Ref_Type'Storage_Size use 0;
   pragma Controlled (Block_Constant_Ref_Type);

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

   function "<="
     (A, B : Block_IO.Valid_Address_Type)
      return Boolean;

   procedure Read
     (File    : in out Block_IO.File_Type;
      Buffer  : in out Buffer_Type;
      Address : in     Block_IO.Valid_Address_Type;
      Block   :    out Block_Type);
   -- Reads an Block from the Buffer or from file if no Block corresponding 
   -- with Address is present in the Buffer. The read Block is not stored
   -- in the Buffer.

   procedure Read
     (File      : in out Block_IO.File_Type;
      Buffer    : in out Buffer_Type;
      Address   : in     Block_IO.Valid_Address_Type;
      Block_Ref :    out Block_Constant_Ref_Type);
   -- Reads an Block from the Buffer or from file if no Block corresponding 
   -- with Address is present in the Buffer. In contrast to the other two
   -- Read subprograms, this function stores the read Block in the Buffer. 
   -- (This is necessary so that the Block will be freed later.)

   procedure Write
     (File     : in out Block_IO.File_Type;
      Buffer   : in out Buffer_Type;
      Address  : in     Block_IO.Valid_Address_Type;
      Block    : in     Block_Type);
   -- Writes an Block to the Buffer, not to the file itself.

private
   type Entry_Type;
   type Entry_Ref_Type is access Entry_Type;
   for Entry_Ref_Type'Storage_Pool use Node_Storage_Pool;

   type Entry_Type is
      record
         Next    : Entry_Ref_Type;
         Address : Block_IO.Valid_Address_Type;
         Block   : Block_Ref_Type;
         Changed : Boolean := False;
      end record;

   type Buffer_Type is
      record
         Head                    : Entry_Ref_Type := null;
         New_Address_Initialized : Boolean := False;
         New_Address             : Block_IO.Valid_Address_Type;
      end record;

end DB.IO.Blocks.Gen_Buffers;

