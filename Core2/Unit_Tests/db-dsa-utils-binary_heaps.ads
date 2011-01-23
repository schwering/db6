-- Abstract:
--
-- Simple binary heap instance.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.DSA.Utils.Gen_Binary_Heaps;

package DB.DSA.Utils.Binary_Heaps is new DB.DSA.Utils.Gen_Binary_Heaps
  (Integer, "<");

