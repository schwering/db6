-- Abstract:
--
-- A generic non-thread-safe dynamic-size stack.
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
-- Copyright 2008--2011 Christoph Schwering

with System.Storage_Pools;

generic
   type Item_Type is private;
   Initial_Size : in Natural;
   Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
package DB.DSA.Utils.Gen_Stacks is
   pragma Preelaborate;

   type Stack_Type is private;

   function New_Stack
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

   procedure Clear
     (Stack : in out Stack_Type);

   function Is_Empty
     (Stack : Stack_Type)
      return Boolean;

   function Size
     (Stack : Stack_Type)
      return Natural;

   function Get
     (Stack : Stack_Type;
      I     : Positive)
      return Item_Type;

   procedure Set
     (Stack : in out Stack_Type;
      I     : in     Positive;
      Item  : in     Item_Type);

   procedure Flip
     (Stack : in out Stack_Type);

private
   type Item_Array_Type is array (Positive range <>) of Item_Type;
   type Item_Array_Ref_Type is access Item_Array_Type;
   for Item_Array_Ref_Type'Storage_Pool use Storage_Pool;

   type Stack_Type is
      record
         Stack_Items : Item_Array_Type (1 .. Initial_Size);
         Heap_Items  : Item_Array_Ref_Type := null;
         Top         : Natural := 0;
      end record;

   pragma Inline (Size);
   pragma Inline (Get);
   pragma Inline (Set);

end DB.DSA.Utils.Gen_Stacks;

