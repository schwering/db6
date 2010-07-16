-- Abstract:
--
-- Generic binary heap implementation.
--
-- Design Notes:
--
-- No heap is used.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic
   type Item_Type is private;
   with function "<" (Left, Right : Item_Type) return Boolean is <>;
package DB.Utils.Gen_Binary_Heaps is
   pragma Pure;

   type Heap_Type (<>) is limited private;

   function New_Heap (Size : Natural) return Heap_Type;
   procedure Extract_Min (Heap : in out Heap_Type; Item : out Item_Type);
   procedure Insert (Heap : in out Heap_Type; Item : in Item_Type);

private
   type Item_Array_Type is array (Positive range <>) of Item_Type;

   type Heap_Type (Size : Natural) is limited
      record
         Buffer : Item_Array_Type (1 .. Size);
         Last   : Natural := 0;
      end record;

   pragma Inline (New_Heap);

end DB.Utils.Gen_Binary_Heaps;

