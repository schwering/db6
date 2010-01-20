-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils;
with DB.Utils.Gen_Minimum;

package body DB.Types.Gen_Strings.Gen_Bounded is

   function Compare
     (Left, Right : String_Type)
      return Utils.Comparison_Result_Type
   is
      function Min is new Utils.Gen_Minimum(Length_Type);
   begin
      for I in 1 .. Min(Left.Length, Right.Length) loop
         declare
            CL : constant Item_Type := Left.Buffer(I);
            CR : constant Item_Type := Right.Buffer(I);
         begin
            if CL /= CR then
               if CL < CR then
                  return Utils.Less;
               else
                  return Utils.Greater;
               end if;
            end if;
         end;
      end loop;

      if Left.Length = Right.Length then
         return Utils.Equal;
      elsif Left.Length < Right.Length then
         return Utils.Less;
      else
         return Utils.Greater;
      end if;
   end Compare;


   function "<" (Left, Right : String_Type) return Boolean is
   begin
      if Left.Length < Right.Length then
         return Left.Buffer(1..Left.Length) <= Right.Buffer(1..Left.Length);
      else
         return Left.Buffer(1..Right.Length) < Right.Buffer(1..Right.Length);
      end if;
   end "<";


   function "<=" (Left, Right : String_Type) return Boolean is
   begin
      if Left.Length <= Right.Length then
         return Left.Buffer(1..Left.Length) <= Right.Buffer(1..Left.Length);
      else
         return Left.Buffer(1..Right.Length) < Right.Buffer(1..Right.Length);
      end if;
   end "<=";


   function "=" (Left, Right : String_Type) return Boolean is
   begin
      return Left.Length = Right.Length and then
             Left.Buffer(1..Left.Length) = Right.Buffer(1..Right.Length);
   end "=";


   function "&" (Left, Right : String_Type) return String_Type
   is
      S : String_Type;
   begin
      S.Length := Left.Length + Right.Length;
      S.Buffer(1 .. S.Length) := Left.Buffer(1 .. Left.Length) &
                                 Right.Buffer(1 .. Right.Length);
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
      S.Length                := Left'Length + Right.Length;
      S.Buffer(1 .. S.Length) := Left & Right.Buffer(1 .. Right.Length);
      return S;
   end "&";


   function To_Index
     (L : Length_Type)
      return Index_Type is
   begin
      return Index_Type(L);
   end To_Index;


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


   function New_String
     (S        : String_Type;
      S_From   : Index_Type;
      S_Length : Length_Type;
      T        : String_Type)
      return String_Type
   is
      SF : Index_Type renames S_From;
      SL : Length_Type renames S_Length;
      TL : Length_Type renames T.Length;
      R  : String_Type;
      pragma Assert (SL = 0 or else          SF in 1 .. S.Length);
      pragma Assert (SL = 0 or else SF + SL - 1 in 1 .. S.Length);
      pragma Assert (SL + TL <= R.Buffer'Length);
   begin
      R.Buffer(1 .. SL)       := S.Buffer(SF .. SF+SL-1);
      R.Buffer(SL+1 .. SL+TL) := T.Buffer(1 .. TL);
      R.Length                := SL + TL;
      pragma Assert (Substring(S, SF, SL) & T = R);
      return R;
   end New_String;


   function Length
     (S : String_Type)
      return Length_Type is
   begin
      return S.Length;
   end Length;


   function Element
     (S : String_Type;
      I : Index_Type)
      return Item_Type is
   begin
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


   function To_Buffer
     (S : String_Type)
      return Indefinite_Buffer_Type is
   begin
      return S.Buffer(1 .. S.Length);
   end To_Buffer;


   function Short_Bound
     (Left  : String_Type)
      return String_Type
   is
      Buffer : Indefinite_Buffer_Type(1 .. Length(Left));
   begin
      for I in 1 .. Length(Left) loop
         if Element(Left, I) < Item_Type'Last then
            Buffer(I) := Item_Type'Succ(Element(Left, I));
            return New_String(Buffer(1 .. I));
         else
            Buffer(I) := Element(Left, I);
         end if;
      end loop;
      return Left;
   end Short_Bound;


   function Short_Delimiter
     (Left  : String_Type;
      Right : String_Type)
      return String_Type
   is
      function Min is new Utils.Gen_Minimum(Length_Type);

      pragma Assert (Left < Right or Left = Right);
      Buffer : Indefinite_Buffer_Type(1 .. Length(Left));
   begin
      for I in 1 .. Min(Length(Left), Length(Right)) loop
         if Element(Left, I) < Item_Type'Last and then
            Item_Type'Succ(Element(Left, I)) < Element(Right, I) then
            Buffer(I) := Item_Type'Succ(Element(Left, I));
            return New_String(Buffer(1 .. I));
         else
            Buffer(I) := Element(Left, I);
         end if;
      end loop;
      return Left;
   end Short_Delimiter;


   package body Uncompressed is separate;
   package body Prefix_Compressed is separate;
   package body Delta_Compressed is separate;
   package body Parted is separate;

end DB.Types.Gen_Strings.Gen_Bounded;

