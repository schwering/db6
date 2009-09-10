package DB.Util.Bitmaps is
   --pragma Pure;

   subtype Size_Type is Natural;
   subtype Index_Type is Natural;
   type Bitmap_Type (<>) is private;

   Invalid_Index : constant Index_Type := Index_Type'Last;

   function New_Bitmap
     (Size : Size_Type)
      return Bitmap_Type;

   procedure Set
     (Bitmap : in out Bitmap_Type;
      Index  : in     Index_Type;
      Value  : in     Boolean);

   function Get
     (Bitmap : Bitmap_Type;
      Index  : Index_Type)
      return Boolean;

   function Next
     (Bitmap : Bitmap_Type;
      Value  : Boolean)
      return Index_Type;

private
   Word_Length : constant := 64;
   type Word_Type is mod 2**Word_Length;
   type Word_Array_Type is array (Index_Type range <>) of Word_Type;
   type Bitmap_Type (Last_Index : Index_Type) is
      record
         Size  : Size_Type;
         Words : Word_Array_Type(0 .. Last_Index);
      end record;

   pragma Inline (Set);
   pragma Inline (Get);

end DB.Util.Bitmaps;

