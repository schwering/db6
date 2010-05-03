-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Utils.Gen_Stacks is

   procedure Free is new Ada.Unchecked_Deallocation
     (Item_Array_Type, Item_Array_Ref_Type);


   function Size
     (Stack : Stack_Type)
      return Natural
   is
      pragma Inline (Size);
   begin
      if Stack.Heap_Items = null then
         return Stack.Stack_Items'Length;
      else
         return Stack.Stack_Items'Length + Stack.Heap_Items'Length;
      end if;
   end Size;


   procedure Resize
     (Stack : in out Stack_Type)
   is
      pragma Inline (Resize);
   begin
      if Stack.Top = Size(Stack) then
         declare
            New_Size       : constant Natural := Size(Stack) * 3 / 2 + 1;
            New_Heap_Items : Item_Array_Ref_Type :=
               new Item_Array_Type(Stack.Stack_Items'Last + 1 .. New_Size);
         begin
            if Stack.Heap_Items /= null then
               New_Heap_Items(Stack.Heap_Items'Range) := Stack.Heap_Items.all;
               Free(Stack.Heap_Items);
            end if;
            Stack.Heap_Items := New_Heap_Items;
            pragma Assert (Size(Stack) = New_Size);
         end;
      end if;
   end Resize;


   function New_Stack
      return Stack_Type is
   begin
      return Stack_Type'(others => <>);
   end New_Stack;


   procedure Finalize
     (Stack : in out Stack_Type) is
   begin
      if Stack.Heap_Items /= null then
         Free(Stack.Heap_Items);
      end if;
   end Finalize;


   procedure Push
     (Stack : in out Stack_Type;
      Item  : in     Item_Type) is
   begin
      Resize(Stack);
      Stack.Top := Stack.Top + 1;
      if Stack.Top <= Stack.Stack_Items'Last then
         Stack.Stack_Items(Stack.Top) := Item;
      else
         Stack.Heap_Items(Stack.Top) := Item;
      end if;
   end Push;


   procedure Pop
     (Stack : in out Stack_Type;
      Item  :    out Item_Type) is
   begin
      if Stack.Top <= Stack.Stack_Items'Last then
         Item := Stack.Stack_Items(Stack.Top);
      else
         Item := Stack.Heap_Items(Stack.Top);
      end if;
      Stack.Top := Stack.Top - 1;
   end Pop;


   function Top
     (Stack : Stack_Type)
      return Item_Type is
   begin
      if Stack.Top <= Stack.Stack_Items'Last then
         return Stack.Stack_Items(Stack.Top);
      else
         return Stack.Heap_Items(Stack.Top);
      end if;
   end Top;


   procedure Clear
     (Stack : in out Stack_Type) is
   begin
      Stack.Top := 0;
   end Clear;


   function Is_Empty
     (Stack : Stack_Type)
      return Boolean is
   begin
      return Stack.Top = 0;
   end Is_Empty;

end DB.Utils.Gen_Stacks;

