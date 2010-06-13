-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.DSA.Utils.Gen_Comparisons is

   function "=" (A, B : Item_Type) return Boolean is
   begin
      return Compare (A, B) = Equal;
   end "=";


   function "<" (A, B : Item_Type) return Boolean is
   begin
      return Compare (A, B) = Less;
   end "<";


   function ">" (A, B : Item_Type) return Boolean is
   begin
      return Compare (A, B) = Greater;
   end ">";


   function "<=" (A, B : Item_Type) return Boolean
   is
      C : constant Comparison_Result_Type := Compare (A, B);
   begin
      return C = Less or C = Equal;
   end "<=";


   function ">=" (A, B : Item_Type) return Boolean
   is
      C : constant Comparison_Result_Type := Compare (A, B);
   begin
      return C = Equal or C = Greater;
   end ">=";

end DB.DSA.Utils.Gen_Comparisons;

