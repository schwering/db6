-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils.Gen_Minimum;

package body DB.Compression.Gen_Prefix is

   function Encode
     (S, T : String_Type)
      return Delta_Type
   is
      function Min is new Utils.Gen_Minimum(Length_Type);

      Prefix_Length : Length_Type := Min(Length(S), Length(T));
   begin
      for I in 1 .. Prefix_Length loop
         if Element(S, To_Index(I)) /= Element(T, To_Index(I)) then
            Prefix_Length := I - 1;
            exit;
         end if;
      end loop;
      if Prefix_Length /= Length(T) then
         declare
            From : constant Index_Type  := To_Index(Prefix_Length + 1);
            Len  : constant Length_Type := Length(T) - Prefix_Length;
            PL : Length_Type renames Prefix_Length;
         begin
            return (Prefix_Length => Prefix_Length,
                    Postfix       => Substring(T, From, Len));
         end;
      else
         return (Prefix_Length => Prefix_Length,
                 Postfix       => Empty_String);
      end if;
   end Encode;


   function Decode
     (S : String_Type;
      D : Delta_Type)
      return String_Type is
   begin
      return New_String(S, 1, D.Prefix_Length, D.Postfix);
   end Decode;

end DB.Compression.Gen_Prefix;

