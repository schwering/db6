-- Abstract:
--
-- Base package for strings of a generic type.
--
-- Copyright 2008--2011 Christoph Schwering

with System.Storage_Elements;

with DB.Utils;

generic
   type Item_Type is (<>);
package DB.Types.Gen_Strings is
   pragma Pure;

   pragma Compile_Time_Error
     (Item_Type'Size /= System.Storage_Elements.Storage_Element'Size,
      "Item_Type in Gen_Strings must have size of byte");
   pragma Assert
     (Item_Type'Size = System.Storage_Elements.Storage_Element'Size);

   subtype Length_Type is Natural range 0 .. Natural'Last;
   subtype Index_Type is Length_Type range 1 .. Length_Type'Last;

   type Indefinite_Buffer_Type is
      array (Positive range <>) of aliased Item_Type;

private
   generic
      Prime : in Positive;
   function Gen_Forward_Hash
     (Buffer : Indefinite_Buffer_Type;
      Last   : Length_Type)
      return Utils.Hash_Type;

   generic
      Prime : in Positive;
   function Gen_Backward_Hash
     (Buffer : Indefinite_Buffer_Type;
      Last   : Length_Type)
      return Utils.Hash_Type;

end DB.Types.Gen_Strings;

