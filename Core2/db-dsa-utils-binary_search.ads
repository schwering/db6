-- Abstract:
--
-- Binary search implementations.
--
-- Design Notes:
--
-- The problem in the Find_Less_Or_Equal*-searches is that we need additional
-- comparisons to assure that if the I-th element is supposed to be the smallest
-- element such that K <= K_I by checking K_{I-1} < K.
--
-- While Uniform_Find_Less_Or_Equal_In_Container2 moves these assertions into
-- the branching body, the Uniform_Find_Less_Or_Equal_In_Container modifies the
-- Compare function so that K = K_I iff K <= K_I and K_{I-1} < K.
--
-- References:
--
-- [Knuth] The Art of Computer Programming: Searching and Sorting (vol 3)
-- (for uniform binary search: pp. 414-415)
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package DB.DSA.Utils.Binary_Search is
   pragma Pure;

   generic
      type Index_Type is range <>;
      type Item_Type is limited private;
      type Array_Type is array (Index_Type range <>) of Item_Type;
      with function "<=" (A, B : Item_Type) return Boolean is <>;
      with function "=" (A, B : Item_Type) return Boolean is <>;
   procedure Find_Equal
     (Arr   : in  Array_Type;
      Item  : in  Item_Type;
      Found : out Boolean;
      Index : out Index_Type);

   generic
      type Container_Type (<>) is limited private;
      type Index_Type is range <>;
      type Item_Type is limited private;
      with function Get (C : Container_Type; I : Index_Type) return Item_Type;
      with function "<=" (A, B : Item_Type) return Boolean is <>;
   procedure Find_Less_Or_Equal
     (Container   : in  Container_Type;
      First_Index : in  Index_Type;
      Last_Index  : in  Index_Type;
      Item        : in  Item_Type;
      Found       : out Boolean;
      Index       : out Index_Type);

   generic
      type Container_Type (<>) is limited private;
      type Extended_Index_Type is range <>;
      Invalid_Index : in Extended_Index_Type;
      type Item_Type is limited private;
      with function Get
             (C : Container_Type;
              I : Extended_Index_Type)
              return Item_Type;
      with function Compare
        (A, B : Item_Type)
         return DB.Utils.Comparison_Result_Type;
   procedure Uniform_Find_Less_Or_Equal
     (Container   : in  Container_Type;
      First_Index : in  Extended_Index_Type;
      Last_Index  : in  Extended_Index_Type;
      Item        : in  Item_Type;
      Index       : out Extended_Index_Type);

   generic
      type Container_Type (<>) is limited private;
      type Extended_Index_Type is range <>;
      Invalid_Index : in Extended_Index_Type;
      type Item_Type is limited private;
      with function Get
        (C : Container_Type;
         I : Extended_Index_Type)
         return Item_Type;
      with function Compare
        (A, B : Item_Type)
         return DB.Utils.Comparison_Result_Type;
   procedure Uniform_Find_Less_Or_Equal2
     (Container   : in  Container_Type;
      First_Index : in  Extended_Index_Type;
      Last_Index  : in  Extended_Index_Type;
      Item        : in  Item_Type;
      Index       : out Extended_Index_Type);

private
   pragma Inline (Find_Equal);
   pragma Inline (Find_Less_Or_Equal);
   pragma Inline (Find_Less_Or_Equal);

end DB.DSA.Utils.Binary_Search;

