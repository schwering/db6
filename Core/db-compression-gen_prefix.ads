-- Abstract:
--
-- String prefix compression.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic
   type Item_Type is (<>);
   type String_Type is private;
   type Index_Type is range <>;
   type Length_Type is range <>;
   Empty_String : in String_Type;
   with function To_Index
          (L : Length_Type)
           return Index_Type;
   with function Length
          (S : String_Type)
           return Length_Type;
   with function Element
          (S : String_Type;
           I : Index_Type)
           return Item_Type;
   with function Substring
          (S      : String_Type;
           From   : Index_Type;
           Length : Length_Type)
           return String_Type;
   with function New_String
          (S        : String_Type;
           S_From   : Index_Type;
           S_Length : Length_Type;
           T        : String_Type)
           return String_Type;
   with function "=" (A, B : Item_Type) return Boolean;
package DB.Compression.Gen_Prefix is
   pragma Preelaborate;

   type Delta_Type is
      record
         Prefix_Length : Length_Type;
         Postfix       : String_Type;
      end record;
   pragma Pack (Delta_Type);

   function Encode
     (S, T : String_Type)
      return Delta_Type;

   function Decode
     (S : String_Type;
      D : Delta_Type)
      return String_Type;

end DB.Compression.Gen_Prefix;

