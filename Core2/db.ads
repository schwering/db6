-- Abstract:
--
-- The root package. Important children are:
--
-- DB.Maps       -- A general object-oriented maps API which essentially
--                  forwards to a Gen_BTrees instance.
-- DB.Gen_BTrees -- A generic Yao & Lehman B-tree for high concurrency.
--                  The core data structure for key/value pairs.
-- DB.Blocks     -- Block IO implementations.
-- DB.Types      -- Key and value types for the Gen_BTrees instance used in
--                  DB.Maps. Would be child package of DB.Maps if the compiler
--                  would accept it (he complains about circular dependencies).
-- DB.Locks      -- Locking mechanisms.
-- DB.Utils      -- Miscellaneous things like a binary search, queues, stacks.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package DB is
   pragma Pure;

   ----------
   -- Exceptions.

   IO_Error : exception;
   -- Indicates a serios IO error.
   -- Raised in DB.IO and its child packages.

   Tree_Error : exception;
   -- This exception is only raised when there are extremely serious
   -- errors in the tree such as dangling references to child or neighbor
   -- nodes.
   -- Raised in DB.Gen_BTrees and DB.Gen_Blob_Trees.

   Timer_Error : exception;
   -- Indicates wrong usage of timers.

   Map_Reduce_Error : exception;
   -- An error occurred during map/reduce.

end DB;

