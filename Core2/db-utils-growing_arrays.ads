-- Abstract:
--
-- Some utility functions for dynamically growing arrays.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package DB.Utils.Growing_Arrays is
   pragma Pure;

   generic
      type Index_Type is range <>;
      type Item_Type is private;
      type Array_Type is array (Index_Type range <>) of Item_Type;
      type Array_Access_Type is access Array_Type;
      Default_Item : in Item_Type;
   procedure Gen_Grow
     (Arr   : in out Array_Access_Type;
      Index : in     Index_Type);

   generic
      type First_Index_Type is range <>;
      type Second_Index_Type is range <>;
      type Item_Type is private;
      type Array_Type is array (First_Index_Type range <>,
                                Second_Index_Type range <>) of Item_Type;
      type Array_Access_Type is access Array_Type;
      Default_Item : in Item_Type;
   procedure Gen_Grow_2d
     (Arr    : in out Array_Access_Type;
      Index1 : in     First_Index_Type;
      Index2 : in     Second_Index_Type);

end DB.Utils.Growing_Arrays;

