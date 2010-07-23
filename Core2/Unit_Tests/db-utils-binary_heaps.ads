-- Abstract:
--
-- Simple binary heap instance.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils.Gen_Binary_Heaps;

package DB.Utils.Binary_Heaps is new DB.Utils.Gen_Binary_Heaps
  (Integer, "<");

