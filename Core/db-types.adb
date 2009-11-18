-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

package body DB.Types is

   function Image (L : Letter_Type) return String is
   begin
      return Letter_Type'Image(L);
   end Image;


   function Image (C : Character) return String is
   begin
      return Character'Image(C);
   end Image;

end DB.Types;

