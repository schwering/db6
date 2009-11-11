with Ada.Unchecked_Deallocation;


package body DB.Types.Gen_Unbounded_Strings is

   overriding
   procedure Initialize (String : in out String_Type) is
   begin
      String.Buffer   := null;
      String.Refcount := new Refcount_Type'(1);
   end;


   overriding
   procedure Adjust (String : in out String_Type) is
   begin
      String.Refcount.all := String.Refcount.all + 1;
   end;


   overriding
   procedure Finalize (String : in out String_Type)
   is
      procedure Free_Buffer is new Ada.Unchecked_Deallocation
        (Buffer_Type, Buffer_Ref_Type);
      procedure Free_Refcount is new Ada.Unchecked_Deallocation
        (Refcount_Type, Refcount_Ref_Type);
   begin
      if String.Refcount /= null and then String.Refcount.all > 1 then
         String.Refcount.all := String.Refcount.all - 1;
      else
         if String.Buffer /= null then
            Free_Buffer(String.Buffer);
         end if;
         if String.Refcount /= null then
            Free_Refcount(String.Refcount);
         end if;
      end if;
   end;


   function "<" (Left, Right : String_Type) return Boolean is
   begin
      return Left.Buffer.all < Right.Buffer.all;
   end "<";


   function "=" (Left, Right : String_Type) return Boolean is
   begin
      return Left.Buffer.all = Right.Buffer.all;
   end "=";


   function "&" (Left, Right : String_Type) return String_Type is
   begin
      return New_String(Left.Buffer.all & Right.Buffer.all);
   end "&";


   function "&"
     (Left  : String_Type;
      Right : Buffer_Type)
      return String_Type is
   begin
      return New_String(Left.Buffer.all & Right);
   end "&";


   function "&"
     (Left  : Buffer_Type;
      Right : String_Type)
      return String_Type is
   begin
      return New_String(Left & Right.Buffer.all);
   end "&";


   function To_Index
     (L : Length_Type)
      return Index_Type is
   begin
      return Index_Type(L);
   end To_Index;


   function Empty_String
     return String_Type
   is
      S : String_Type;
   begin
      S.Buffer := new Buffer_Type(1 .. 0);
      return S;
   end Empty_String;


   function New_String
     (Arr : Buffer_Type)
      return String_Type
   is
      S : String_Type;
   begin
      S.Buffer := new Buffer_Type'(Arr);
      return S;
   end New_String;


   function Length
     (S : String_Type)
      return Length_Type is
   begin
      return S.Buffer'Length;
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
      return String_Type is
   begin
      return New_String(S.Buffer.all(From .. From + Length - 1));
   end Substring;


   function To_String
     (S : String_Type)
      return Buffer_Type is
   begin
      return S.Buffer.all(1 .. S.Length);
   end To_String;


   package body Uncompressed is separate;
   package body Prefix_Compressed is separate;

end DB.Types.Gen_Unbounded_Strings;

