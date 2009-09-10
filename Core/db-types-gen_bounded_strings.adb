package body DB.Types.Gen_Bounded_Strings is

   function "<" (Left, Right : Indefinite_Buffer_Type) return Boolean
   is
      pragma Inline ("<");
   begin
      for I in Left'Range loop
         if Left(I) < Right(I) then
            return True;
         elsif Left(I) > Right(I) then
            return False;
         end if;
      end loop;
      return False;
   end "<";


   function "<=" (Left, Right : Indefinite_Buffer_Type) return Boolean
   is
      pragma Inline ("<=");
   begin
      for I in Left'Range loop
         if Left(I) < Right(I) then
            return True;
         elsif Left(I) > Right(I) then
            return False;
         end if;
      end loop;
      return True;
   end "<=";


   function "<" (Left, Right : String_Type) return Boolean
   is begin
      if Left.Length < Right.Length then
         return Left.Buffer(1..Left.Length) <= Right.Buffer(1..Left.Length);
      else
         return Left.Buffer(1..Right.Length) < Right.Buffer(1..Right.Length);
      end if;

   end "<";


   function "=" (Left, Right : String_Type) return Boolean
   is begin
      return Left.Length = Right.Length and then
             Left.Buffer(1..Left.Length) = Right.Buffer(1..Right.Length);
   end "=";


   function "&" (Left, Right : String_Type) return String_Type
   is
      S : String_Type;
   begin
      S.Length := Left.Length + Right.Length;
      S.Buffer(1 .. S.Length) := Left.Buffer(1 .. Left.Length)
                               & Right.Buffer(1 .. Right.Length);
      return S;
   end "&";


   function "&"
     (Left  : String_Type;
      Right : Indefinite_Buffer_Type)
      return String_Type
   is
      S : String_Type;
   begin
      S.Length := Left.Length + Right'Length;
      S.Buffer(1 .. S.Length) := Left.Buffer(1 .. Left.Length) & Right;
      return S;
   end "&";


   function "&"
     (Left  : Indefinite_Buffer_Type;
      Right : String_Type)
      return String_Type
   is
      S : String_Type;
   begin
      S.Length := Left'Length + Right.Length;
      S.Buffer(1 .. S.Length) := Left & Right.Buffer(1 .. Right.Length);
      return S;
   end "&";


   function To_Index
     (L : Length_Type)
      return Index_Type
   is begin
      return Index_Type(L);
   end To_Index;


   function Empty_String
     return String_Type
   is
      S : String_Type;
   begin
      S.Length := 0;
      return S;
   end Empty_String;


   function New_String
     (Arr : Indefinite_Buffer_Type)
      return String_Type
   is
      S : String_Type;
   begin
      S.Buffer(1 .. Arr'Length) := Arr;
      S.Length                  := Arr'Length;
      return S;
   end New_String;


   function Length
     (S : String_Type)
      return Length_Type
   is begin
      return S.Length;
   end Length;


   function Element
     (S : String_Type;
      I : Index_Type)
      return Item_Type
   is begin
      return S.Buffer(I);
   end Element;


   function Substring
     (S      : String_Type;
      From   : Index_Type;
      Length : Length_Type)
      return String_Type
   is
      T : String_Type;
   begin
      T.Buffer(1 .. Length) := S.Buffer(From .. From + Length - 1);
      T.Length              := Length;
      return T;
   end Substring;


   function To_String
     (S : String_Type)
      return Indefinite_Buffer_Type
   is begin
      return S.Buffer(1 .. S.Length);
   end To_String;


   package body Uncompressed is separate;
   package body Prefix_Compressed is separate;
   package body Delta_Compressed is separate;

end DB.Types.Gen_Bounded_Strings;
