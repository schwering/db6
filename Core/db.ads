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
-- Copyright 2008, 2009 Christoph Schwering

package DB is
   pragma Pure;
end DB;

