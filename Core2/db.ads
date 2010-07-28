-- Abstract:
--
-- The root package. Here's a quick overview of the most important packages:
--
-- DB.Blocks             -- Serialization of data and block IO.
--
-- DB.DSA                -- Generic first-class data structures and algorithms.
-- DB.DSA.Gen_BTrees     -- Generic B-tree structure.
-- DB.DSA.Gen_Map_Reduce -- Generic simple MapReduce implementation.
-- DB.DSA.Utils          -- Helpers needed by the stuff in DB.DSA.
--
-- DB.Maps               -- A general object-oriented maps API.
-- DB.Types              -- Key and value types for the Gen_BTrees instance used
--                          in DB.Maps. Would be child package of DB.Maps if the
--                          compiler would accept it (he complains about
--                          circular dependencies).
--
-- DB.Utils              -- Data structures that are rather trivial and other
--                          helpers.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package DB is
   pragma Pure;

   ----------
   -- Exceptions.

   Underflow_Error, Overflow_Error : exception;
   -- Indicate container (e.g. array) under- and overflows.
   -- Mainly used in the DB.Utils package.

   IO_Error : exception;
   -- Indicates a serios IO error.
   -- Raised in DB.IO and its child packages.

   Tree_Error : exception;
   -- This exception is only raised when there are extremely serious
   -- errors in the tree such as dangling references to child or neighbor
   -- nodes.
   -- Raised in DB.DSA.Gen_BTrees and DB.Gen_Blob_Trees.

   Timer_Error : exception;
   -- Indicates wrong usage of timers.

   Map_Reduce_Error : exception;
   -- An error occurred during map/reduce.

   Tag_Error : exception;
   -- Raised on tag (in OO-sense) error.

end DB;

