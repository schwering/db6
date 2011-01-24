-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;

package body DB.DSA.Utils.Gen_Small_LRU_Caches.Gen_Test is

   procedure Set_Up (T : in out Test_Type)
   is
      pragma Unreferenced (T);
   begin
      null;
   end;


   procedure Tear_Down (T : in out Test_Type)
   is
      pragma Unreferenced (T);
   begin
      null;
   end;


   procedure Check_Size (C : in out Cache_Type)
   is
      S : Natural := 0;
      N : Node_Ref_Type := C.Head;
   begin
      while N /= null loop
         S := S + 1;
         if N.Next = null then
            Assert (N = C.Tail, "last node isn't tail");
         end if;
         N := N.Next;
      end loop;
      Assert (S = C.Size, "size inconsistent: "& S'Img &" /= "& C.Size'Img);
   end Check_Size;


   procedure Tests (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      K : Key_Type := Initial_Key;
      V : Value_Type := Initial_Value;
      C : Cache_Type;
   begin
      Assert (C.Size = 0, "Initial cache not empty");
      Check_Size (C);

      Put (C, K, V);
      Assert (C.Head.Key = K, "least recently used item not at top");
      Check_Size (C);
      declare
         VV : Value_Type;
         F  : Boolean;
      begin
         Get (C, K, VV, F);
         Assert (F, "item "& Key_Img (K) &" not found");
         Assert (C.Head.Key = K, "least recently used item not at top");
         Assert (C.Tail.Key = K, "least recently used item not at top");
         Assert (V = VV, "Not matching value found: "&
                 Value_Img (V) &" /= "& Value_Img (VV));
      end;
      Assert (C.Size = 1, "Cache.Size not consistent");
      Check_Size (C);

      -- fill first half

      K := Initial_Key;
      V := Initial_Value;
      for I in 1 .. Slot_Count / 2 loop
         Put (C, K, V);
         Assert (C.Head.Key = K, "least recently used item not at top "& I'Img);
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;
      Assert (C.Size = Slot_Count / 2, "Cache.Size not consistent");
      Check_Size (C);

      K := Initial_Key;
      V := Initial_Value;
      for I in 1 .. Slot_Count / 2 loop
         Put (C, K, V);
         Assert (C.Head.Key = K, "least recently used item not at top");
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;
      Assert (C.Size = Slot_Count / 2, "Cache.Size not consistent "&
              "after double insertion");
      Check_Size (C);

      K := Initial_Key;
      V := Initial_Value;
      for I in 1 .. Slot_Count / 2 loop
         declare
            VV : Value_Type;
            F  : Boolean;
         begin
            Get (C, K, VV, F);
            Assert (F, "item "& Key_Img (K) &" not found");
            Assert (C.Head.Key = K, "least recently used item not at top");
            Assert (V = VV, "Not matching value found: "&
                    Value_Img (V) &" /= "& Value_Img (VV));
         end;
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;

      -- fill second half

      for I in Slot_Count / 2 + 1 .. Slot_Count loop
         Put (C, K, V);
         Assert (C.Head.Key = K, "least recently used item not at top");
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;
      Assert (C.Size = Slot_Count, "Cache.Size not consistent");
      Check_Size (C);

      K := Initial_Key;
      V := Initial_Value;
      for I in 1 .. Slot_Count loop
         Put (C, K, V);
         Assert (C.Head.Key = K, "least recently used item not at top");
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;
      Assert (C.Size = Slot_Count, "Cache.Size not consistent "&
              "after double insertion");
      Check_Size (C);

      K := Initial_Key;
      V := Initial_Value;
      for I in 1 .. Slot_Count loop
         declare
            VV : Value_Type;
            F  : Boolean;
         begin
            Get (C, K, VV, F);
            Assert (F, "item "& Key_Img (K) &" not found");
            Assert (C.Head.Key = K, "least recently used item not at top");
            Assert (V = VV, "Not matching value found: "&
                    Value_Img (V) &" /= "& Value_Img (VV));
         end;
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;

      -- now try replacement 

      K := Initial_Key;
      V := Initial_Value;
      for I in 1 .. Slot_Count loop
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;
      for I in Slot_Count + 1 .. 2 * Slot_Count loop
         Put (C, K, V);
         Assert (C.Head.Key = K, "least recently used item not at top");
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;
      Assert (C.Size = Slot_Count, "Cache.Size not consistent");
      Check_Size (C);

      K := Initial_Key;
      V := Initial_Value;
      for I in 1 .. Slot_Count loop
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;
      for I in Slot_Count + 1 .. 2 * Slot_Count loop
         declare
            VV : Value_Type;
            F  : Boolean;
         begin
            Get (C, K, VV, F);
            Assert (F, "item "& Key_Img (K) &" not found "& I'Img);
            Assert (C.Head.Key = K, "least recently used item not at top");
            Assert (V = VV, "Not matching value found: "&
                    Value_Img (V) &" /= "& Value_Img (VV));
         end;
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;
      Assert (C.Size = Slot_Count, "Cache.Size not consistent");
      Check_Size (C);

      -- try it reverse one time

      K := Initial_Key;
      V := Initial_Value;
      for I in 1 .. Slot_Count loop
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;
      for I in reverse Slot_Count + 1 .. 2 * Slot_Count loop
         declare
            VV : Value_Type;
            F  : Boolean;
         begin
            Get (C, K, VV, F);
            Assert (F, "item "& Key_Img (K) &" not found "& I'Img);
            Assert (C.Head.Key = K, "least recently used item not at top");
            Assert (V = VV, "Not matching value found: "&
                    Value_Img (V) &" /= "& Value_Img (VV));
         end;
         K := Next_Key (K);
         V := Next_Value (V);
      end loop;
      Assert (C.Size = Slot_Count, "Cache.Size not consistent");
      Check_Size (C);

   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Tests;

end DB.DSA.Utils.Gen_Small_LRU_Caches.Gen_Test;

