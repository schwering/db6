-- Abstract:
-- 
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Tags;

package body DB.Tables is

   function Equal_Values (Left, Right : Value_Type'Class) return Boolean
   is
      use type Ada.Tags.Tag;
   begin
      return Left'Tag = Right'Tag and then
             Left.Equals(Right);
   end Equal_Values;

end DB.Tables;

