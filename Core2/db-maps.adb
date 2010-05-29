-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Tags;

with DB.Maps.Bounded;

package body DB.Maps is

   function Equals
     (Left, Right : Comparable_Type'Class)
      return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      return Left'Tag = Right'Tag and then
             Left.Equals(Right);
   end Equals;


   function New_Map
     (Max_Key_Size   : in Blocks.Size_Type;
      Max_Value_Size : in Blocks.Size_Type)
      return Map_Type'Class is
   begin
      return Bounded.New_Map;
   end New_Map;


   function Positive_Infinity_Bound
      return Bound_Type is
   begin
      return Bound_Type'(Concrete => False, Infinity => Positive_Infinity);
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound
      return Bound_Type is
   begin
      return Bound_Type'(Concrete => False, Infinity => Negative_Infinity);
   end Negative_Infinity_Bound;


   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type is
   begin
      return Bound_Type'(Concrete   => True,
                         Comparison => Comparison,
                         Key        => Key);
   end New_Bound;

end DB.Maps;

