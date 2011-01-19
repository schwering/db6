-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Utils.Gen_Binary_Heaps is

   function New_Heap (Capacity : Natural) return Heap_Type is
   begin
      return Heap_Type'(Capacity => Capacity, Last => 0, others => <>);
   end New_Heap;


   function Parent (Index : Positive) return Positive
   is
      pragma Inline (Parent);
   begin
      return Index / 2;
   end Parent;


   function Left_Child (Index : Positive) return Positive
   is
      pragma Inline (Left_Child);
   begin
      return 2 * Index;
   end Left_Child;


   function Right_Child (Index : Positive) return Positive
   is
      pragma Inline (Right_Child);
   begin
      return 2 * Index + 1;
   end Right_Child;


   function Min_Child (Heap : Heap_Type; Index : Positive) return Positive
   is
      pragma Inline (Min_Child);
      L : constant Positive := Left_Child (Index);
      R : constant Positive := Right_Child (Index);
   begin
      if L = Heap.Last then
         return L;
      elsif Heap.Buffer (L) < Heap.Buffer (R) then
         return L;
      else
         return R;
      end if;
   end Min_Child;


   function Has_Children
     (Heap  : Heap_Type;
      Index : Positive)
      return Boolean
   is
      pragma Inline (Has_Children);
   begin
      return 2 * Index > Heap.Last;
   end Has_Children;


   procedure Swap (Heap : in out Heap_Type; I, J : in Positive)
   is
      pragma Inline (Swap);
      Tmp : constant Item_Type := Heap.Buffer (I);
   begin
      Heap.Buffer (I) := Heap.Buffer (J);
      Heap.Buffer (J) := Tmp;
   end Swap;


   procedure Heapify (Heap : in out Heap_Type; Index : Positive)
   is
      I : Positive := Index;
   begin
      loop
         exit when I = 1 or else
                   Heap.Buffer (Parent (I)) < Heap.Buffer (I);
         Swap (Heap, I, Parent (I));
         I := Parent (I);
      end loop;

      loop
         exit when Has_Children (Heap, I);
         declare
            Min : constant Positive := Min_Child (Heap, I);
         begin
            exit when Heap.Buffer (I) < Heap.Buffer (Min);
            Swap (Heap, I, Min);
            I := Min;
         end;
      end loop;
   end Heapify;


   procedure Extract_Min (Heap : in out Heap_Type; Item : out Item_Type) is
   begin
      if Heap.Last < 1 then
         raise Underflow_Error;
      end if;

      Item := Heap.Buffer (1);
      if Heap.Last > 1 then
         Heap.Buffer (1) := Heap.Buffer (Heap.Last);
         Heapify (Heap, 1);
      end if;
      Heap.Last := Heap.Last - 1;
   end Extract_Min;


   procedure Insert (Heap : in out Heap_Type; Item : in Item_Type) is
   begin
      if Heap.Last >= Heap.Capacity then
         raise Overflow_Error;
      end if;

      Heap.Last := Heap.Last + 1;
      Heap.Buffer (Heap.Last) := Item;
      Heapify (Heap, Heap.Last);
   end Insert;


   procedure Clear
     (Heap : in out Heap_Type;
      Free : access procedure (Item : in out Item_Type)) is
   begin
      if Free /= null then
         for I in 1 .. Heap.Last loop
            Free (Heap.Buffer (I));
         end loop;
      end if;
      Heap.Last := 0;
   end Clear;


   function Is_Empty (Heap : Heap_Type) return Boolean is
   begin
      return Heap.Last = 0;
   end Is_Empty;


   function Size (Heap : Heap_Type) return Natural is
   begin
      return Heap.Last;
   end Size;


   function Get (Heap : Heap_Type; I : Positive) return Item_Type is
   begin
      return Heap.Buffer (I);
   end Get;


   function Min (Heap : Heap_Type) return Item_Type is
   begin
      return Get (Heap, Heap.Buffer'First);
   end Min;


   function Capacity (Heap : Heap_Type) return Natural is
   begin
      return Heap.Capacity;
   end Capacity;

end DB.Utils.Gen_Binary_Heaps;

