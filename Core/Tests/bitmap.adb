with DB.Util.Bitmaps;
use DB.Util.Bitmaps;

procedure Bitmap
is
   Size   : constant := 70;
   Bitmap : Bitmap_Type := New_Bitmap(Size);
   subtype Valid_Range is Index_Type
      range Index_Type'First .. Index_Type'First + Size - 1;
begin
   for I in Valid_Range loop
      pragma Assert (Get(Bitmap, I) = False);
      Set(Bitmap, I, True);
      pragma Assert (Get(Bitmap, I) = True);
      pragma Assert (I = Valid_Range'Last or else Next(Bitmap, False) = I + 1);
   end loop;
   for I in Valid_Range loop
      pragma Assert (Get(Bitmap, I) = True);
      Set(Bitmap, I, False);
      pragma Assert (Get(Bitmap, I) = False);
      pragma Assert (I = Valid_Range'Last or else Next(Bitmap, True) = I + 1);
   end loop;
end Bitmap;

