-- Abstract:
--
-- Root package of miscellaneous utilities.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package DB.Utils is
   pragma Pure;

   subtype Size_Type is Natural;
   type Hash_Type is mod 2**32;

end DB.Utils;

