package body DB.Util.Bitmaps is

   function Index_To_Word
     (Index : Index_Type)
      return Index_Type
   is begin
      return Index / Word_Length;
   end Index_To_Word;


   function Index_To_Bit
     (Index : Index_Type)
      return Index_Type
   is begin
      return Index mod Word_Length;
   end Index_To_Bit;


   function New_Bitmap
     (Size : Size_Type)
      return Bitmap_Type
   is begin
      return Bitmap_Type'(Last_Index => (Size+Word_Length-1) / Word_Length - 1,
                          Size       => Size,
                          Words      => (others => 0));
   end New_Bitmap;


   procedure Set
     (Bitmap : in out Bitmap_Type;
      Index  : in     Index_Type;
      Value  : in     Boolean)
   is
      W : constant Index_Type := Index_To_Word(Index);
      B : constant Index_Type := Index_To_Bit(Index);
   begin
      if Index >= Bitmap.Size then
         raise Constraint_Error;
      end if;
      case Value is
         when True =>
            Bitmap.Words(W) := Bitmap.Words(W) or 2**B;
         when False =>
            Bitmap.Words(W) := Bitmap.Words(W) and (not (2**B));
      end case;
   end Set;


   function Get
     (Bitmap : Bitmap_Type;
      Index  : Index_Type)
      return Boolean
   is
      W : constant Index_Type := Index_To_Word(Index);
      B : constant Index_Type := Index_To_Bit(Index);
   begin
      if Index >= Bitmap.Size then
         raise Constraint_Error;
      end if;
      return (Bitmap.Words(W) and 2**B) /= 0;
   end Get;


   function Next
     (Bitmap : Bitmap_Type;
      Value  : Boolean)
      return Index_Type
   is begin
      for W in Bitmap.Words'Range loop
         if (Value and then Bitmap.Words(W) /= 0) or
            (not Value and then Bitmap.Words(W) /= (not 0)) then
            for B in 0 .. Word_Length - 1 loop
               if ((Bitmap.Words(W) and 2**B) /= 0) = Value then
                  declare
                     I : constant Index_Type := W * Word_Length + B;
                  begin
                     if I >= Bitmap.Size then
                        raise Constraint_Error;
                     end if;
                     return I;
                  end;
               end if;
            end loop;
         end if;
      end loop;
      return Invalid_Index;
   end Next;

end DB.Util.Bitmaps;

