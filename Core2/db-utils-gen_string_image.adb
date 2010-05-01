with Ada.Unchecked_Conversion;
with System;

with DB.Blocks;

function DB.Utils.Gen_String_Image (O : Object_Type) return String
is
   Size : constant Blocks.Size_Type
        := Blocks.Bits_To_Units(Object_Type'Size);
   type Def_String is new String(1 .. Natural(Size));
   for Def_String'Component_Size use System.Storage_Unit;
   function Convert is new Ada.Unchecked_Conversion(Object_Type, Def_String);
begin
   return String(Convert(O));
end DB.Utils.Gen_String_Image;

