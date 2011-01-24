-- Abstract:
--
-- Test utilities.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Utils;

package DB.DSA.Utils.Small_LRU_Caches_Test_Utils is

   subtype Key_Type is Integer;
   subtype Value_Type is Integer;

   Initial : Integer := 0;

   function Hash (I : Integer) return DB.Utils.Hash_Type;
   function Succ (I : Integer) return Integer;
   function Image (I : Integer) return String;

end DB.DSA.Utils.Small_LRU_Caches_Test_Utils;

