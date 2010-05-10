-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Utils.Gen_Stacks is

   procedure Free is new Ada.Unchecked_Deallocation
     (Item_Array_Type, Item_Array_Ref_Type);


   function Capacity
     (Stack : Stack_Type)
      return Natural
   is
      pragma Inline (Capacity);
   begin
      if Stack.Heap_Items = null then
         return Stack.Stack_Items'Length;
      else
         return Stack.Stack_Items'Length + Stack.Heap_Items'Length;
      end if;
   end Capacity;


   procedure Resize
     (Stack : in out Stack_Type)
   is
      pragma Inline (Resize);
   begin
      if Stack.Top = Capacity(Stack) then
         declare
            New_Capacity   : constant Natural := Capacity(Stack) * 3 / 2 + 1;
            New_Heap_Items : Item_Array_Ref_Type :=
               new Item_Array_Type(Stack.Stack_Items'Last + 1 .. New_Capacity);
         begin
            if Stack.Heap_Items /= null then
               New_Heap_Items(Stack.Heap_Items'Range) := Stack.Heap_Items.all;
               Free(Stack.Heap_Items);
            end if;
            Stack.Heap_Items := New_Heap_Items;
            pragma Assert (Capacity(Stack) = New_Capacity);
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


   function Size
     (Stack : Stack_Type)
      return Natural is
   begin
      return Stack.Top;
   end Size;


   function Get
     (Stack : Stack_Type;
      I     : Positive)
      return Item_Type is
   begin
      if I <= Stack.Stack_Items'Last then
         return Stack.Stack_Items(I);
      else
         return Stack.Heap_Items(I);
      end if;
   end Get;


   procedure Set
     (Stack : in out Stack_Type;
      I     : in     Positive;
      Item  : in     Item_Type) is
   begin
      if I <= Stack.Stack_Items'Last then
         Stack.Stack_Items(I) := Item;
      else
         Stack.Heap_Items(I) := Item;
      end if;
   end Set;


   procedure Flip
     (Stack : in out Stack_Type)
   is
      procedure Swap (I, J : Positive)
      is
         Tmp : Item_Type := Get(Stack, I);
      begin
         Set(Stack, I, Get(Stack, J));
         Set(Stack, J, Tmp);
      end Swap;
   begin
      for I in 1 .. Size(Stack) / 2 loop
         Swap(I, Size(Stack) - I + 1);
      end loop;
   end Flip;

end DB.Utils.Gen_Stacks;

