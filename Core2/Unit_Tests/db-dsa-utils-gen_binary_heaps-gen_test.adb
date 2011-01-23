-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;
with Ada.Exceptions; use Ada.Exceptions;

with AUnit.Assertions; use AUnit.Assertions;

package body DB.DSA.Utils.Gen_Binary_Heaps.Gen_Test is

   Heap_Size : constant := 100;

   procedure Set_Up (T : in out Test_Type) is
   begin
      T.Last := Initial_Item;
      T.Heap := new Heap_Type'(New_Heap (Heap_Size));
   end;


   procedure Tear_Down (T : in out Test_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Heap_Type, Heap_Ref_Type);
   begin
      Free (T.Heap);
   end;


   procedure Extract_Min (T : in out Test_Type'Class) is
   begin
      if Size (T.Heap.all) >= 2 then
         declare
            Cnt  : constant Natural := Size (T.Heap.all);
            I, J : Item_Type;
         begin
            Extract_Min (T.Heap.all, I);
            Extract_Min (T.Heap.all, J);
            Assert (I < J, "Violates I = "& Img (I) &" < J = "& Img (J));
            Insert (T.Heap.all, I);
            Assert (Cnt = Size (T.Heap.all) + 1,
                    "Size"& Cnt'Img &" /="& Natural'Image (Size (T.Heap.all)));
         end;
      else
         declare
            I : Item_Type;
         begin
            Extract_Min (T.Heap.all, I);
            Extract_Min (T.Heap.all, I);
            Assert (False, "Underflow should have been raised");
         exception
            when Underflow_Error =>
               null;
            when E : others =>
               Assert (False,
                       "Wrong exception raised "& Exception_Information (E));
         end;
      end if;
   end;


   procedure Insert (T : in out Test_Type'Class) is
   begin
      T.Last := Succ (T.Last);
      if Size (T.Heap.all) < Capacity (T.Heap.all) then
         Insert (T.Heap.all, T.Last);
      else
         declare
         begin
            Insert (T.Heap.all, T.Last);
            Assert (False, "Overflow should have been raised");
         exception
            when Overflow_Error =>
               null;
            when E : others =>
               Assert (False,
                       "Wrong exception raised "& Exception_Information (E));
         end;
      end if;
   end;


   procedure Clear (T : in out Test_Type'Class)
   is
      Cnt : Natural := 0;
      procedure Free (I : in out Item_Type)
      is
         pragma Unreferenced (I);
      begin
         Cnt := Cnt + 1;
      end;
      Old_Size : constant Natural := Size (T.Heap.all);
   begin
      Clear (T.Heap.all, Free'Access);
      Assert (Size (T.Heap.all) = 0,
              "Size"& Natural'Image (Size (T.Heap.all)) &" /= 0");
      Assert (Is_Empty (T.Heap.all), "Heap is not empty");
      Assert (Cnt = Old_Size,
              "Only"& Cnt'Img &" /="& Old_Size'Img &" items freed");
   end;


   procedure Is_Empty (T : in out Test_Type'Class) is
   begin
      for I in 1 .. Size (T.Heap.all) loop
         declare
            I : Item_Type;
         begin
            Extract_Min (T.Heap.all, I);
         end;
      end loop;
      Assert (Is_Empty (T.Heap.all), "Heap is not empty");
   end;


   procedure Inserts (T : in out Test_Type'Class) is
   begin
      for I in 1 .. Heap_Size + 10 loop
         Insert (T);
      end loop;
   end;


   procedure Extract_Mins (T : in out Test_Type'Class) is
   begin
      for I in 1 .. Heap_Size + 10 loop
         Insert (T);
      end loop;
   end;


   procedure Tests (T : in out Test_Type) is
   begin
      Extract_Mins (T);
      Inserts (T);
      Clear (T);
      Is_Empty (T);
   end Tests;

end DB.DSA.Utils.Gen_Binary_Heaps.Gen_Test;

