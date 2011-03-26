-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;

package body DB.Blocks.Test is

   overriding
   procedure Set_Up (T : in out Test_Type) is
   begin
      null;
   end Set_Up;


   overriding
   procedure Tear_Down (T : in out Test_Type) is
   begin
      null;
   end Tear_Down;


   procedure Test_Cursor (T : in out Test_Type)
   is
      pragma Unreferenced (T);

      procedure Test (B : in out Base_Block_Type)
      is
         subtype Byte is Storage_Element_Type;
         use type Byte;
         function Size_Of is new Blocks.Size_Of (Byte);
         procedure Write is new Blocks.Write (Byte);
         procedure Read is new Blocks.Read (Byte);

         C : Cursor_Type := New_Cursor (B, B'First);
      begin
         Assert (Is_Valid (C), "Cursor not valid");
         Assert (Position (C) = B'First, "Initial Position is wrong");
         Assert (Remaining_Space (C) = B'Length, "Remaining_Space is wrong");

         Restrict (C, Position (C) - 1);
         Assert (Remaining_Space (C) = 0, "Restrict didn't work");
         Unrestrict (C, B);
         Restrict (C, B'Length / 2);
         Assert (Remaining_Space (C) = B'Length / 2, "Restrict didn't work "&
                 Size_Type'Image (Remaining_Space (C)) &" /= "&
                 Size_Type'Image (B'Length / 2));
         Unrestrict (C, B);
         Assert (Remaining_Space (C) = B'Length, "Unrestrict didn't work");
         Restrict (C, B'Length * 2);
         Assert (Remaining_Space (C) = B'Length, "Remaining_Space not "&
                 "restricted to block length");

         for I in B'Range loop
            Write (B, C, Byte'Last);
            Assert (Is_Valid (C), "Cursor not valid");
            Assert (Remaining_Space (C) = Size_Type (B'Length - I),
                    "Remaining_Space not correct");

            declare
               D : Cursor_Type := New_Cursor (B, B'First);
               X : Byte;
            begin
               Reset_Free_Space_Of_Block (B, Position (C));
               for J in B'First .. I loop
                  Read (B, D, X);
                  Assert (Is_Valid (D), "Cursor not valid");
                  Assert (X = Byte'Last, "Read wrong item "& X'Img);
               end loop;
               for J in I + 1 .. B'Last loop
                  Read (B, D, X);
                  Assert (Is_Valid (D), "Cursor not valid");
                  Assert (X = 0, "Read wrong item "& X'Img);
               end loop;
            end;

            declare
               D : Cursor_Type := New_Cursor (B, B'First);
               X : Byte;
            begin
               Restrict (D, Position (C) - 1);
               for J in B'First .. I loop
                  Read (B, D, X);
                  Assert (Is_Valid (D), "Cursor not valid");
                  Assert (X = Byte'Last, "Read wrong item "& X'Img);
               end loop;
               declare
                  E : Cursor_Type := D;
               begin
                  for J in I + 1 .. B'Last loop
                     Read (B, E, X);
                     Assert (not Is_Valid (E),
                             "Cursor valid after read in restricted area");
                     exit;
                  end loop;
               end;
               declare
                  E : Cursor_Type := D;
               begin
                  X := Byte'Last;
                  for J in I + 1 .. B'Last loop
                     Write (B, E, X);
                     Assert (not Is_Valid (E),
                             "Cursor valid after write in restricted area");
                     exit;
                  end loop;
               end;
               Unrestrict (D, B);

               for J in I + 1 .. B'Last loop
                  Read (B, D, X);
                  Assert (Is_Valid (D), "Cursor not valid");
                  Assert (X = 0, "Read wrong item "& X'Img);
               end loop;
            end;
         end loop;
      end Test;

      B1 : Block_Type;
      B2 : Base_Block_Type (Index_Type'First .. Index_Type'Last * 3 / 2);
   begin
      Test (B1);
      Test (B2);
   end Test_Cursor;


   procedure Test_Utils (T : in out Test_Type)
   is
      pragma Unreferenced (T);
   begin
      Assert (Bits_To_Units (0) = 0, "Bits_To_Units failed for 0");
      for I in Size_Type (1) .. Size_Type (8) loop
         Assert (Bits_To_Units (I) = 1, "Bits_To_Units failed for "& I'Img);
      end loop;
      for I in Size_Type (9) .. Size_Type (16) loop
         Assert (Bits_To_Units (I) = 2, "Bits_To_Units failed for "& I'Img);
      end loop;
   end Test_Utils;

end DB.Blocks.Test;

