-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Types.Gen_Strings is

   function Gen_Forward_Hash
     (Buffer : Indefinite_Buffer_Type;
      Last   : Length_Type)
      return Utils.Hash_Type
   is
      use type Utils.Hash_Type;
      P : constant Utils.Hash_Type := Utils.Hash_Type (Prime);
      H : Utils.Hash_Type := 0;
   begin
      for I in Buffer'First .. Last loop
         declare
            F : constant Utils.Hash_Type := P**(I - Buffer'First);
            B : System.Storage_Elements.Storage_Element;
            for B'Address use Buffer (I)'Address;
         begin
            exit when F = 0; -- all subsequent summands are zero
            H := H + Utils.Hash_Type (B) * F;
         end;
      end loop;
      return H;
   end Gen_Forward_Hash;


   function Gen_Backward_Hash
     (Buffer : Indefinite_Buffer_Type;
      Last   : Length_Type)
      return Utils.Hash_Type
   is
      use type Utils.Hash_Type;
      P : constant Utils.Hash_Type := Utils.Hash_Type (Prime);
      H : Utils.Hash_Type := 0;
   begin
      for I in reverse Buffer'First .. Last loop
         declare
            F : constant Utils.Hash_Type := P**(Last - (I - Buffer'First));
            B : System.Storage_Elements.Storage_Element;
            for B'Address use Buffer (I)'Address;
         begin
            exit when F = 0; -- all subsequent summands are zero
            H := H + Utils.Hash_Type (B) * F;
         end;
      end loop;
      return H;
   end Gen_Backward_Hash;

end DB.Types.Gen_Strings;

