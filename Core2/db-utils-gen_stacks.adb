-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Utils.Gen_Stacks is

   procedure Free is new Ada.Unchecked_Deallocation
     (Item_Array_Type, Item_Array_Ref_Type);


   procedure Resize
     (Stack : in out Stack_Type)
   is
      pragma Inline (Resize);
   begin
      if Stack.Top = Stack.Items'Length then
         declare
            New_Size  : constant Natural := Stack.Items'Length * 3 / 2 + 1;
            New_Items : Item_Array_Ref_Type := new Item_Array_Type(1..New_Size);
         begin
            New_Items(Stack.Items'Range) := Stack.Items.all;
            Free(Stack.Items);
            Stack.Items := New_Items;
         end;
      end if;
   end Resize;


   function New_Stack
     (Initial_Size : Positive)
      return Stack_Type is
   begin
      return Stack_Type'(new Item_Array_Type(1 .. Initial_Size), 0);
   end New_Stack;


   procedure Finalize
     (Stack : in out Stack_Type) is
   begin
      Free(Stack.Items);
   end Finalize;


   procedure Push
     (Stack : in out Stack_Type;
      Item  : in     Item_Type) is
   begin
      Resize(Stack);
      Stack.Top := Stack.Top + 1;
      Stack.Items(Stack.Top) := Item;
   end Push;


   procedure Pop
     (Stack : in out Stack_Type;
      Item  :    out Item_Type) is
   begin
      Item := Stack.Items(Stack.Top);
      Stack.Top := Stack.Top - 1;
   end Pop;


   function Top
     (Stack : Stack_Type)
      return Item_Type is
   begin
      return Stack.Items(Stack.Top);
   end Top;


   function Is_Empty
     (Stack : Stack_Type)
      return Boolean is
   begin
      return Stack.Top = 0;
   end Is_Empty;

end DB.Utils.Gen_Stacks;

