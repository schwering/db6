-- Abstract:
--
-- Root package of miscellaneous utilities.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils;

package DB.DSA.Utils is
   pragma Pure;

   subtype Comparison_Result_Type is DB.Utils.Comparison_Result_Type;
   use type Comparison_Result_Type;

   Less    : Comparison_Result_Type renames DB.Utils.Less;
   Equal   : Comparison_Result_Type renames DB.Utils.Equal;
   Greater : Comparison_Result_Type renames DB.Utils.Greater;

end DB.DSA.Utils;

