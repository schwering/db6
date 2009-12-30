with Ada.Text_IO; use Ada.Text_IO;
with Interfaces;

procedure I_BM
is
   Count : constant := 2**10;
   type Word_Type is new Interfaces.Unsigned_64;
   Word_Size : constant := Word_Type'Size;
   subtype Index_Type is Integer range 1 .. Count;
   type Bitmap_Type is array (Index_Type) of Word_Type;
   
   Bitmap_Error : exception;

   Empty_Bitmap : constant Bitmap_Type := (others => 0);

   procedure Set (BM : in out Bitmap_Type; I : in Index_Type; V : in Boolean)
   is
      pragma Inline (Set);
      BM_I : constant Index_Type := I / Word_Size;
      Word : constant Word_Type  := Shift_Left(1, Natural(I mod Word_Size));
   begin
      if V then
         BM(BM_I) := BM(I / Word_Size) or Word;
      else
         BM(BM_I) := BM(I / Word_Size) and not Word;
      end if;
   end Set;

   function Get (BM : Bitmap_Type; I : Index_Type) return Boolean
   is
      pragma Inline (Get);
      BM_I : constant Index_Type := I / Word_Size;
      Word : constant Word_Type  := Shift_Left(1, Natural(I mod Word_Size));
   begin
      return (BM(BM_I) and Word) /= 0;
   end Get;

   BM : Bitmap_Type := Empty_Bitmap;
begin
   for I in 1 .. BM'Length * Word_Size loop
      Set(BM, I, True);
      for J in 1 .. BM'Length * Word_Size loop
         if J <= I and then Get(BM, J) = False then
            Put_Line("J ="& Integer'Image(J));
            Put_Line("J != True, J = "& Boolean'Image(Get(BM, J)));
            raise Bitmap_Error;
         elsif J > I and then Get(BM, J) then
            Put_Line("J ="& Integer'Image(J));
            Put_Line("J != False, J = "& Boolean'Image(Get(BM, J)));
            raise Bitmap_Error;
         end if;
      end loop;
   end loop;
end I_BM;

