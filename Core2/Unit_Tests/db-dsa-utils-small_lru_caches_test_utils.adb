-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.DSA.Utils.Small_LRU_Caches_Test_Utils is

   function Hash (I : Integer) return DB.Utils.Hash_Type is
   begin
      return DB.Utils.Hash_Type (abs (I));
   end Hash;


   function Succ (I : Integer) return Integer is
   begin
      if I < 0 then
         return (+1) * (I**2) + 1;
      else
         return (-1) * (I**2) - 1;
      end if;
   end Succ;


   function Image (I : Integer) return String
   is
      S : constant String := Integer'Image (I);
   begin
      if S'Length > 0 and then S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Image;

end DB.DSA.Utils.Small_LRU_Caches_Test_Utils;

