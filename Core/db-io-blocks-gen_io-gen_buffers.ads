-- Abstract:
--
-- A non-persistent layer for IO operations that can be materialized.
-- This is used by writing transactions: they work on a buffer which is
-- materialized on a commit and simply forgotten on an abort.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic

   pragma Warnings (Off); -- Disable `unreferenced' warnings

   type Buffer_Type is private;

   with function New_Buffer
      return Buffer_Type;
   -- Creates a new buffer.

   with procedure Free
     (Buffer : in out Buffer_Type);
   -- Frees all resources hold by the buffer.

   with procedure Commit
     (File   : in out File_Type;
      Buffer : in     Buffer_Type);
   -- Flushes all dirty blocks in the buffer. See Write and Read.

   with procedure Seek_New
     (File    : in out File_Type;
      Buffer  : in out Buffer_Type;
      Address :    out Valid_Address_Type);
   -- Sets Address to the value it would be set to by Seek_New if
   -- those blocks that are currently in the Buffer were written to file
   -- directly.

   with procedure Read
     (File    : in out File_Type;
      Buffer  : in out Buffer_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type);
   -- Reads an Block from the Buffer or from file if no Block corresponding 
   -- with Address is present in the Buffer. The read Block is not stored
   -- in the Buffer.

   with procedure Write
     (File     : in out File_Type;
      Buffer   : in out Buffer_Type;
      Address  : in     Valid_Address_Type;
      Block    : in     Block_Type);
   -- Writes an Block to the Buffer, not to the file itself.

   pragma Warnings (On);

package DB.IO.Blocks.Gen_IO.Gen_Buffers is
   pragma Pure;
end DB.IO.Blocks.Gen_IO.Gen_Buffers;

