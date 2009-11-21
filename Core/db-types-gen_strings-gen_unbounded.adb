-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

with Ada.Unchecked_Deallocation;

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
   procedure Finalize (String : in out String_Type)
   is
   begin
      if String.S /= null and then String.S.Refcount > 1 then
         String.S.Refcount := String.S.Refcount - 1;
      else
         declare
            procedure Free_Bounded_String is new Ada.Unchecked_Deallocation
              (Bounded_String_Type, Bounded_String_Ref_Type);
         begin
            Free_Bounded_String(String.S);
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
         return New_String(Left.S.Buffer & Right.S.Buffer);
      end if;
   end "&";


   function "&"
     (Left  : String_Type;
      Right : Indefinite_Buffer_Type)
      return String_Type is
   begin
      if Left.S = null then
         return New_String(Right);
      else
         return New_String(Left.S.Buffer & Right);
      end if;
   end "&";


   function "&"
     (Left  : Indefinite_Buffer_Type;
      Right : String_Type)
      return String_Type is
   begin
      if Right.S = null then
         return New_String(Left);
      else
         return New_String(Left & Right.S.Buffer);
      end if;
   end "&";


   function To_Index
     (L : Length_Type)
      return Index_Type is
   begin
      return Index_Type(L);
   end To_Index;


   function New_String
     (Arr : Indefinite_Buffer_Type)
      return String_Type is
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


   function Length
     (S : String_Type)
      return Length_Type is
   begin
      if S.S = null then
         return 0;
      else
         return S.S.Buffer'Length;
      end if;
   end Length;


   function Element
     (S : String_Type;
      I : Index_Type)
      return Item_Type is
   begin
      return S.S.Buffer(I);
   end Element;


   function Substring
     (S      : String_Type;
      From   : Index_Type;
      Length : Length_Type)
      return String_Type is
   begin
      return New_String(S.S.Buffer(From .. From + Length - 1));
   end Substring;


   function To_Buffer
     (S : String_Type)
      return Indefinite_Buffer_Type is
   begin
      if S.S = null then
         declare
            Empty_Buffer : constant Indefinite_Buffer_Type(1 .. 0)
                         := (others => Item_Type'First);
         begin
            return Empty_Buffer;
         end;
      else
         return S.S.Buffer(1 .. S.Length);
      end if;
   end To_Buffer;


   package body Uncompressed is separate;
   package body Prefix_Compressed is separate;

end DB.Types.Gen_Strings.Gen_Unbounded;

