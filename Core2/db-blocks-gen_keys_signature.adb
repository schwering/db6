-- Abstract:
--
-- see spec.
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Blocks.Gen_Keys_Signature is

   function "=" (A, B : Key_Type) return Boolean
   is
      pragma Unreferenced (A);
      pragma Unreferenced (B);
   begin
      return False;
   end "=";

end DB.Blocks.Gen_Keys_Signature;

