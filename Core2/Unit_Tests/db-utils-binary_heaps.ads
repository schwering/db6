-- Abstract:
--
-- Simple binary heap instance.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Utils.Gen_Binary_Heaps;

package DB.Utils.Binary_Heaps is new DB.Utils.Gen_Binary_Heaps
  (Integer, "<");

