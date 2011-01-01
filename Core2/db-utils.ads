-- Abstract:
--
-- Root package of miscellaneous utilities.
--
-- Copyright 2008--2011 Christoph Schwering

package DB.Utils is
   pragma Pure;

   subtype Size_Type is Natural;
   type Hash_Type is mod 2**32;

   type Comparison_Result_Type is (Less, Equal, Greater);

end DB.Utils;

