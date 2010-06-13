-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Tags;

with DB.Maps.Bounded;

package body DB.Maps is

   function Equals (Left, Right : Comparable_Type'Class) return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      return Left'Tag = Right'Tag and then
             Left.Equals (Right);
   end Equals;


   function New_Map
     (Implementation   : in String;
      Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type'Class is
   begin
      if Implementation = "btree" then
         return Bounded.New_Map (Allow_Duplicates);
      else
         raise Program_Error;
      end if;
   end New_Map;


   function New_Map
     (Max_Key_Size     : in Blocks.Size_Type;
      Max_Value_Size   : in Blocks.Size_Type;
      Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type'Class
   is
      use type Blocks.Size_Type;
   begin
      if Max_Key_Size <= Bounded.Max_Key_Size (Max_Value_Size) then
         return Bounded.New_Map (Allow_Duplicates);
      else
         raise Program_Error;
      end if;
   end New_Map;


   function Positive_Infinity_Bound return Bound_Type is
   begin
      return Bound_Type' (Concrete => False, Infinity => Positive_Infinity);
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound return Bound_Type is
   begin
      return Bound_Type' (Concrete => False, Infinity => Negative_Infinity);
   end Negative_Infinity_Bound;


   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type is
   begin
      return Bound_Type' (Concrete   => True,
                          Comparison => Comparison,
                          Key        => Key);
   end New_Bound;

end DB.Maps;

