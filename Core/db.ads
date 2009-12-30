-- Abstract:
--
-- The root package. Important children are:
--
-- DB.Gen_BTrees -- The core data structure for key/value pairs.
-- DB.Gen_Heaps  -- Data structure for large values.
-- DB.IO.Blocks  -- Block IO implementations.
-- DB.Types      -- Key and value types for the BTree, including strings.
-- DB.Locks      -- Locking mechanisms.
-- DB.Utils      -- Miscellaneous things.
-- DB.Tables     -- Table wrapper.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package DB is
   pragma Pure;

   ----------
   -- Exceptions.

   IO_Error : exception;
   -- Indicates a serios IO error.
   -- Raised in DB.IO and its child packages.

   Lock_Error : exception;
   -- Indicates a wrong usage of a lock.
   -- Raised in DB.Locks and its child packages.

   Tree_Error : exception;
   -- This exception is only raised when there are extremely serious
   -- errors in the tree such as dangling references to child or neighbor
   -- nodes.
   -- Raised in DB.Gen_BTrees and DB.Gen_Blob_Trees.

   Node_Error : exception renames Tree_Error;
   -- Raised for errors that occur in the node management of a tree.
   -- Raised in DB.Gen_BTrees private nested package Nodes.

   Heap_Error : exception;
   -- This exception is only raised when there are extremely serious
   -- errors in the heap such as dangling references to child or neighbor
   -- nodes.
   -- Raised in DB.Gen_Heaps.

   Timer_Error : exception;
   -- Indicates wrong usage of timers.

   Hash_Table_Error : exception;
   -- An error in the fixed hash table.

   Map_Reduce_Error : exception;
   -- An error occurred during map/reduce.

end DB;

