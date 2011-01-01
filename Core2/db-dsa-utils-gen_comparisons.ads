-- Abstract:
--
-- Creates simple comparison functions based on a Compare function.
--
-- Copyright 2008--2011 Christoph Schwering

generic
   type Item_Type (<>) is limited private;
   with function Compare (A, B : Item_Type) return Comparison_Result_Type;
package DB.DSA.Utils.Gen_Comparisons is
   pragma Pure;

   function "=" (A, B : Item_Type) return Boolean;
   function "<" (A, B : Item_Type) return Boolean;
   function ">" (A, B : Item_Type) return Boolean;
   function "<=" (A, B : Item_Type) return Boolean;
   function ">=" (A, B : Item_Type) return Boolean;

end DB.DSA.Utils.Gen_Comparisons;

