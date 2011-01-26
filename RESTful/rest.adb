-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

package body REST is

   function Make_Key
     (Key : String;
      Max : Boolean := False)
      return DB.Types.Keys.Key_Type
   is
      use DB.Types.Keys;
   begin
      if Max then
         return (Row    => Rows.New_String (Rows.Indefinite_Buffer_Type (Key)),
                 Column => Columns.Max_String,
                 Time   => Times.Number_Type'Last);
      else
         return (Row    => Rows.New_String (Rows.Indefinite_Buffer_Type (Key)),
                 Column => Columns.Empty_String,
                 Time   => Times.Number_Type'First);
      end if;
   end Make_Key;


   function Img (N : Integer) return String
   is
      S : constant String := Integer'Image (N);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Img;

end REST;

