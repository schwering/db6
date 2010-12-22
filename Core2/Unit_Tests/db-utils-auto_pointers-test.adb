-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Assertions; use AUnit.Assertions;

with DB.Utils.Debug_Storage_Pools;
with DB.Utils.Gen_Auto_Pointers;

package body DB.Utils.Auto_Pointers.Test is

   procedure Test_Memory_Management (T : in out Test_Type)
   is
      pragma Unreferenced (T);

      Item_SP : Debug_Storage_Pools.Debug_Storage_Pool_Type;
      --Refcount_SP : Debug_Storage_Pools.Debug_Storage_Pool_Type;
      -- XXX This doesn't work because I can't set a (new) formal generic
      -- parameter for the refcount storage pool to this value due to Adas rules
      -- (or a bug in GNAT?)

      subtype Item_Type is Integer;
      type Item_Ref_Type is access Item_Type;
      --for Item_Ref_Type'Storage_Pool use SP;

      package Auto_Pointers is new Gen_Auto_Pointers
        (Item_Type             => Item_Type,
         Item_Ref_Type         => Item_Ref_Type);
         --Refcount_Storage_Pool => Refcount_SP);
      use Auto_Pointers;
   begin
      declare
         A : constant Auto_Pointer_Type := New_Auto_Pointer (null);
         B : constant Auto_Pointer_Type := New_Auto_Pointer (3);
         C : Auto_Pointer_Type;
         D : constant Auto_Pointer_Type := A;
         E : constant Auto_Pointer_Type := B;
      begin
                       Assert (Ref (A) = null, "Ref A /= null");
                       Assert (Ref (D) = null, "Ref D /= null");
                       Assert (Ref (E) /= Ref (B), "Refs E = B");
                       Assert (Ref (E).all = Ref (B).all, "Refs.all E /= B");

         C := A;       Assert (Ref (C) = null, "Ref C /= null");
                       Assert (Ref (A) = null, "Ref C /= null");
         C := B;       Assert (Ref (C) /= Ref (B), "Refs C = B");
                       Assert (Ref (C).all = Ref (B).all, "Refs.all C /= B");
      end;

      Assert (Item_SP.Count = 0,
              "The diff of item allocations and deallocations is "&
              Integer'Image (Item_SP.Count));
      --Assert (Refcount_SP.Count = 0,
              --"The diff of refcount allocations and deallocations is "&
              --Integer'Image (Refcount_SP.Count));
   end Test_Memory_Management;

end DB.Utils.Auto_Pointers.Test;

