-- Abstract:
--
-- Root package of miscellaneous utilities.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Utils;

package DB.DSA.Utils is
   pragma Pure;

   subtype Comparison_Result_Type is DB.Utils.Comparison_Result_Type;

   Less    : Comparison_Result_Type renames DB.Utils.Less;
   Equal   : Comparison_Result_Type renames DB.Utils.Equal;
   Greater : Comparison_Result_Type renames DB.Utils.Greater;

private
   use type Comparison_Result_Type;

end DB.DSA.Utils;

