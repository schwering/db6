generic
   type Item_Type is private;
   with function "=" (A, B : Item_Type) return Boolean is <>;
   type Item_Array_Type is array (Positive range <>) of Item_Type;
   Max_Length : in Positive;
package DB.Compression.Levenshtein is
   pragma Pure;

   subtype Distance_Type is Natural;
   subtype Length_Type is Natural;
   subtype Invalid_Index_Type is Integer;
   subtype Index_Type is Invalid_Index_Type range 1 .. Max_Length;
   type Action_Type is (Keep, Replace, Insert, Delete);

   type Long_Action_Type is
      record
         Action : Action_Type;
         Index  : Index_Type;
         Length : Index_Type;
      end record;
   pragma Pack (Long_Action_Type);

   type Long_Action_Array_Type is array (Positive range <>) of Long_Action_Type;
   pragma Pack (Long_Action_Array_Type);

   type Delta_Type (Action_Count, Char_Count : Length_Type) is
      record
         Actions : Long_Action_Array_Type(1 .. Action_Count);
         Chars   : Item_Array_Type(1 .. Char_Count);
      end record;
   pragma Pack (Delta_Type);



   function Distance
     (S, T : Item_Array_Type)
      return Distance_Type;

   function Encode
     (S, T : Item_Array_Type)
      return Delta_Type;

   function Decode
     (S : Item_Array_Type;
      D : Delta_Type)
      return Item_Array_Type;

private
   subtype Invalid_Matrix_Index_Type is Integer;
   subtype Matrix_Index_Type is Invalid_Matrix_Index_Type;
   type Distance_Matrix_Type is
      array (Matrix_Index_Type range <>, Matrix_Index_Type range <>)
      of Distance_Type;

   type Action_Array_Type is array (Positive range <>) of Action_Type;
   pragma Pack (Action_Array_Type);

end DB.Compression.Levenshtein;

