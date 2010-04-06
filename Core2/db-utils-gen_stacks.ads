-- Abstract:
--
-- A dynamically growing stack. Uses the standard heap.
--
-- Design Notes:
--
-- The stack regrows by the factor of 1.5 each time it is overfull. 
-- The Storage_Pool is used for the array memory. This means that the complete
-- stack must be copied to the new array each time the stack is resized. On the
-- other hand, this means the count of single allocations and deallocations is
-- minimized and there is no overhead as long as the items on the stack are
-- (about as) large as pointers to them.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Storage_Pools;
with DB.Utils.Global_Pool;

generic
   type Item_Type is private;
   Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
package DB.Utils.Gen_Stacks is
   pragma Preelaborate;

   type Stack_Type is private;

   function New_Stack
     (Initial_Size : Positive)
      return Stack_Type;

   procedure Finalize
     (Stack : in out Stack_Type);

   procedure Push
     (Stack : in out Stack_Type;
      Item  : in     Item_Type);

   procedure Pop
     (Stack : in out Stack_Type;
      Item  :    out Item_Type);

   function Top
     (Stack : Stack_Type)
      return Item_Type;

   function Is_Empty
     (Stack : Stack_Type)
      return Boolean;

private
   type Item_Array_Type is array (Positive range <>) of Item_Type;
   type Item_Array_Ref_Type is access Item_Array_Type;

   type Stack_Type is
      record
         Items : Item_Array_Ref_Type := null;
         Top   : Natural := 0;
      end record;

end DB.Utils.Gen_Stacks;

