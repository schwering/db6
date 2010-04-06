-- Abstract:
--
-- Base package for strings of a generic type.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic
   type Item_Type is (<>);
package DB.Types.Gen_Strings is
   pragma Pure;

   subtype Length_Type is Natural range 0 .. Natural'Last;
   subtype Index_Type is Length_Type range 1 .. Length_Type'Last;

   type Indefinite_Buffer_Type is array (Positive range <>) of Item_Type;

end DB.Types.Gen_Strings;

