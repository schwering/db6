-- Abstract:
--
-- Generic Bloom filter implementation.
--
-- If Absent (Item) returns True, then Item is guaranteed to be absent. On the
-- other hand, if Absent (Item) returns False, Item might still be present.
--
-- If Contains (Item) returns True, the item may be absent (the reason is hash
-- collisions). If Contains (Item) returns False, this answer is fool-proof.
-- In other words, Contains (Item) is simply the negation of Absent (Item).
--
-- Concurrent operations are safe.
--
-- Design Notes:
--
-- No heap is used.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Utils;

generic
   type Item_Type is private;
   with function Hash_1 (Item : Item_Type) return DB.Utils.Hash_Type;
   with function Hash_2 (Item : Item_Type) return DB.Utils.Hash_Type;
   with function Hash_3 (Item : Item_Type) return DB.Utils.Hash_Type;
   with function Hash_4 (Item : Item_Type) return DB.Utils.Hash_Type;
   Size_In_Bytes : in Positive;
package DB.DSA.Utils.Gen_Bloom_Filters is
   pragma Pure;

   type Bloom_Filter_Type is limited private;

   procedure Reset (Filter : in out Bloom_Filter_Type);
   procedure Insert (Filter : in out Bloom_Filter_Type; Item : in Item_Type);
   function Contains
     (Filter : Bloom_Filter_Type; Item : Item_type)
      return Boolean;
   function Absent
     (Filter : Bloom_Filter_Type; Item : Item_type)
      return Boolean;

private
   subtype Hash_Type is DB.Utils.Hash_Type;
   use type Hash_Type;

   Size : constant Hash_Type := Hash_Type (Size_In_Bytes) * 8;
   Last : constant Hash_Type := Size - 1;

   type Boolean_Array_Type is array (0 .. Last) of Boolean;
   pragma Pack (Boolean_Array_Type);

   type Bloom_Filter_Type is limited
      record
         Mask : Boolean_Array_Type;
      end record;

   pragma Inline (Absent);

end DB.DSA.Utils.Gen_Bloom_Filters;

