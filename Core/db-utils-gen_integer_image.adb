with Ada.Unchecked_Conversion;
with System;

with DB.IO.Blocks;

function DB.Utils.Gen_Integer_Image (O : Object_Type) return String
is
   use type IO.Blocks.Size_Type;
   pragma Warnings (Off); -- Object_Type'Size warnings
   function Convert_To_Integer is
      new Ada.Unchecked_Conversion(Object_Type, Integer);
   function Convert_To_Long_Integer is
      new Ada.Unchecked_Conversion(Object_Type, Long_Integer);
   pragma Warnings (On);
   Size : constant IO.Blocks.Size_Type
        := IO.Blocks.Bits_To_Units(Object_Type'Size);
begin
   if Size = IO.Blocks.Bits_To_Units(Integer'Size) then
      return Integer'Image(Convert_To_Integer(O));
   elsif Size = IO.Blocks.Bits_To_Units(Long_Integer'Size) then
      return Long_Integer'Image(Convert_To_Long_Integer(O));
   else
      raise Program_Error;
   end if;
end DB.Utils.Gen_Integer_Image;

