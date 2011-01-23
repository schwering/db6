-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body DB.Types.Gen_Strings.Gen_Unbounded is

   overriding
   procedure Initialize (String : in out String_Type) is
   begin
      String.S := null;
   end;


   overriding
   procedure Adjust (String : in out String_Type) is
   begin
      if String.S /= null then
         String.S.Refcount := String.S.Refcount + 1;
      end if;
   end;


   overriding
   procedure Finalize (String : in out String_Type) is
   begin
      if String.S /= null and then String.S.Refcount > 1 then
         String.S.Refcount := String.S.Refcount - 1;
      else
         declare
            procedure Free_Bounded_String is new Ada.Unchecked_Deallocation
              (Bounded_String_Type, Bounded_String_Ref_Type);
         begin
            Free_Bounded_String (String.S);
         end;
      end if;
   end;


   function "<" (Left, Right : String_Type) return Boolean is
   begin
      if Left.S = null and Right.S = null then
         return False;
      elsif Left.S = null then
         return True;
      elsif Right.S = null then
         return False;
      else
         return Left.S.Buffer < Right.S.Buffer;
      end if;
   end "<";


   function "<=" (Left, Right : String_Type) return Boolean is
   begin
      if Left.S = null and Right.S = null then
         return True;
      elsif Left.S = null then
         return True;
      elsif Right.S = null then
         return False;
      else
         return Left.S.Buffer <= Right.S.Buffer;
      end if;
   end "<=";


   function "=" (Left, Right : String_Type) return Boolean is
   begin
      if Left.S = Right.S then
         return True;
      elsif Left.S /= null and Right.S /= null then
         return Left.S.Buffer = Right.S.Buffer;
      else
         return False;
      end if;
   end "=";


   function "&" (Left, Right : String_Type) return String_Type is
   begin
      if Left.S = null and Right.S = null then
         return Empty_String;
      elsif Left.S = null then
         return Right;
      elsif Right.S = null then
         return Left;
      else
         return New_String (Left.S.Buffer & Right.S.Buffer);
      end if;
   end "&";


   function "&"
     (Left  : String_Type;
      Right : Indefinite_Buffer_Type)
      return String_Type is
   begin
      if Left.S = null then
         return New_String (Right);
      else
         return New_String (Left.S.Buffer & Right);
      end if;
   end "&";


   function "&"
     (Left  : Indefinite_Buffer_Type;
      Right : String_Type)
      return String_Type is
   begin
      if Right.S = null then
         return New_String (Left);
      else
         return New_String (Left & Right.S.Buffer);
      end if;
   end "&";


   function To_Index (L : Length_Type) return Index_Type is
   begin
      return Index_Type (L);
   end To_Index;


   function New_String (Arr : Indefinite_Buffer_Type) return String_Type is
   begin
      if Arr'Length = 0 then
         return Empty_String;
      else
         return String_Type'(Ada.Finalization.Controlled with
                             new Bounded_String_Type'(Length   => Arr'Length,
                                                      Refcount => 1,
                                                      Buffer   => Arr));
      end if;
   end New_String;


   function New_String (Length : Length_Type) return String_Type is
   begin
      if Length = 0 then
         return Empty_String;
      else
         return String_Type'(Ada.Finalization.Controlled with
                             new Bounded_String_Type'(Length   => Length,
                                                      Refcount => 1,
                                                      others   => <>));
      end if;
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
      R  : String_Type := New_String (SL + TL);
   begin
      R.S.Buffer (1 .. SL)         := S.S.Buffer (SF .. SF+SL-1);
      R.S.Buffer (SL+1 .. SL+TL-1) := T.S.Buffer (1 .. TL);
      return R;
   end New_String;


   function Length (S : String_Type) return Length_Type is
   begin
      if S.S = null then
         return 0;
      else
         return S.S.Buffer'Length;
      end if;
   end Length;


   function Element (S : String_Type; I : Index_Type) return Item_Type is
   begin
      return S.S.Buffer (I);
   end Element;


   function Substring
     (S      : String_Type;
      From   : Index_Type;
      Length : Length_Type)
      return String_Type is
   begin
      return New_String (S.S.Buffer (From .. From + Length - 1));
   end Substring;


   function To_Buffer (S : String_Type) return Indefinite_Buffer_Type is
   begin
      if S.S = null then
         declare
            Empty_Buffer : constant Indefinite_Buffer_Type (1 .. 0)
                         := (others => Item_Type'First);
         begin
            return Empty_Buffer;
         end;
      else
         return S.S.Buffer (1 .. S.Length);
      end if;
   end To_Buffer;


   function Hash_1 (S : String_Type) return Utils.Hash_Type is
      function Hash is new Gen_Forward_Hash (31);
      pragma Inline (Hash);
   begin
      return Hash (S.S.Buffer, S.Length);
   end Hash_1;


   function Hash_2 (S : String_Type) return Utils.Hash_Type is
      function Hash is new Gen_Backward_Hash (31);
      pragma Inline (Hash);
   begin
      return Hash (S.S.Buffer, S.Length);
   end Hash_2;


   function Hash_3 (S : String_Type) return Utils.Hash_Type is
      function Hash is new Gen_Forward_Hash (29);
      pragma Inline (Hash);
   begin
      return Hash (S.S.Buffer, S.Length);
   end Hash_3;


   function Hash_4 (S : String_Type) return Utils.Hash_Type is
      function Hash is new Gen_Backward_Hash (29);
      pragma Inline (Hash);
   begin
      return Hash (S.S.Buffer, S.Length);
   end Hash_4;


   function Image (S : String_Type) return String is
      function Convert is new Ada.Unchecked_Conversion (Item_Type, Character);
      use Ada.Strings.Unbounded;
      T : Unbounded_String;
   begin
      if S.S = null then
         return "";
      end if;
      for I in 1 .. S.Length loop
         Append (T, Convert (S.S.Buffer (I)));
      end loop;
      return To_String (T);
   end Image;


   package body Uncompressed is separate;

end DB.Types.Gen_Strings.Gen_Unbounded;

