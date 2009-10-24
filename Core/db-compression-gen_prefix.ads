generic
   type Item_Type is private;
   type String_Type is private;
   type Index_Type is range <>;
   type Length_Type is range <>;
   with function Empty_String
           return String_Type;
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
   with function "=" (A, B : Item_Type) return Boolean;
   with function "&" (A, B : String_Type) return String_Type;
package DB.Compression.Gen_Prefix is
   pragma Pure;

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

