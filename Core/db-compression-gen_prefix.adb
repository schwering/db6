-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

package body DB.Compression.Gen_Prefix is

   function Encode
     (S, T : String_Type)
      return Delta_Type
   is
      function Min (X, Y : Length_Type) return Length_Type is
      begin
         if X < Y then
            return X;
         else
            return Y;
         end if;
      end Min;

      Prefix_Length : Length_Type := 0;
   begin
      for I in 1 .. Min(Length(S), Length(T)) loop
         if Element(S, To_Index(I)) = Element(T, To_Index(I)) then
            Prefix_Length := Prefix_Length + 1;
         else
            exit;
         end if;
      end loop;
      if Prefix_Length /= Length(T) then
         declare
            From : constant Index_Type  := To_Index(Prefix_Length + 1);
            Len  : constant Length_Type := Length(T) - Prefix_Length;
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
      return Substring(S, 1, D.Prefix_Length) & D.Postfix;
   end Decode;

end DB.Compression.Gen_Prefix;

