-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with AUnit.Assertions; use AUnit.Assertions;

with DB.Utils.Debug_Storage_Pools;
with DB.Utils.Gen_Smart_Pointers;

package body DB.Utils.Smart_Pointers.Test is

   procedure Test_Memory_Management (T : in out Test_Type)
   is
      pragma Unreferenced (T);

      Item_SP     : Debug_Storage_Pools.Debug_Storage_Pool_Type;
      --Refcount_SP : Debug_Storage_Pools.Debug_Storage_Pool_Type;
      -- XXX This doesn't work because I can't set a (new) formal generic
      -- parameter for the refcount storage pool to this value due to Adas rules
      -- (or a bug in GNAT?)

      subtype Item_Type is Integer;
      type Item_Ref_Type is access Item_Type;
      --for Item_Ref_Type'Storage_Pool use SP;

      package Smart_Pointers is new Gen_Smart_Pointers
        (Item_Type             => Item_Type,
         Item_Ref_Type         => Item_Ref_Type);
         --Refcount_Storage_Pool => Refcount_SP);
      use Smart_Pointers;
   begin
      declare
         A : Smart_Pointer_Type := New_Smart_Pointer (null);
         B : aliased Smart_Pointer_Type := New_Smart_Pointer (new Item_Type);
         C : Smart_Pointer_Type;
         D : Smart_Pointer_Type := A;
         E : Smart_Pointer_Type := B;
         Ptr : constant access Smart_Pointer_Type := B'Access;
      begin
                       Assert (Ref (A) = null, "Ref A /= null");
                       Assert (Ref (D) = Ref (A), "Refs D /= A");
                       Assert (Ref (E) = Ref (B), "Refs E /= B");

         C := A;       Assert (Ref (C) = Ref (A), "Refs C /= A");
         A := B;       Assert (Ref (A) = Ref (B), "Refs A /= B");
         D := A;       Assert (Ref (D) = Ref (A), "Refs D /= A");
         C := E;       Assert (Ref (C) = Ref (E), "Refs C /= E");
         Ptr.all := B; Assert (Ref (Ptr.all) = Ref (B), "Refs Ptr.all /= B");
                       Assert (Ref (Ptr.all) /= null, "Ref Ptr.all = null");

         A := B;       Assert (Ref (A) = Ref (B), "Refs A /= B");
         pragma Warnings (Off);
         B := B;       Assert (Ref (B) = Ref (B), "Refs B /= B");
         pragma Warnings (On);
         Ptr.all := B; Assert (Ref (Ptr.all) = Ref (B), "Refs Ptr.all /= B");
         C := B;       Assert (Ref (C) = Ref (B), "Refs C /= B");
         D := B;       Assert (Ref (D) = Ref (B), "Refs D /= B");
         E := B;       Assert (Ref (E) = Ref (B), "Refs E /= B");
      end;

      Assert (Item_SP.Count = 0,
              "The diff of item allocations and deallocations is "&
              Integer'Image (Item_SP.Count));
      --Assert (Refcount_SP.Count = 0,
              --"The diff of refcount allocations and deallocations is "&
              --Integer'Image (Refcount_SP.Count));
   end Test_Memory_Management;

end DB.Utils.Smart_Pointers.Test;

