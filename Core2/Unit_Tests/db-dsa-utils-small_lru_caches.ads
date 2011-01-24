-- Abstract:
--
-- Simple LRU cache instance.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.DSA.Utils.Gen_Small_LRU_Caches;
with DB.DSA.Utils.Small_LRU_Caches_Test_Utils;

package DB.DSA.Utils.Small_LRU_Caches is new DB.DSA.Utils.Gen_Small_LRU_Caches
  (Integer,
   Integer,
   Hash => Small_LRU_Caches_Test_Utils.Hash,
   Slot_Count => 10);

