-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;

with DB.Types.Strings;
with DB.Types.Strings.Bounded;
with DB.Utils;

package body DB.Types.Strings_Bounded.Test is
   use DB.Types.Strings.Bounded;

   overriding
   procedure Set_Up (T : in out Test_Type) is
   begin
      null;
   end Set_Up;


   overriding
   procedure Tear_Down (T : in out Test_Type) is
   begin
      null;
   end Tear_Down;


   procedure Test_Hash (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      use type DB.Utils.Hash_Type;
      Kurz : constant String := "kurz";
      Lang : constant String := "Ein langer dummer ganz ganz langer "&
                                "und dabei total doofer super dummer "&
                                "ganz-zahl-ueberlauf-erzwingender "&
                                "String voller Moeglichkeiten fuer die "&
                                "kleinen Hashfunktionen";
      K : constant String_Type := New_String (Indefinite_Buffer_Type (Kurz));
      L : constant String_Type := New_String (Indefinite_Buffer_Type (Lang));
      K_H1 : constant DB.Utils.Hash_Type := Hash_1 (K);
      K_H2 : constant DB.Utils.Hash_Type := Hash_2 (K);
      K_H3 : constant DB.Utils.Hash_Type := Hash_3 (K);
      K_H4 : constant DB.Utils.Hash_Type := Hash_4 (K);
      L_H1 : constant DB.Utils.Hash_Type := Hash_1 (L);
      L_H2 : constant DB.Utils.Hash_Type := Hash_2 (L);
      L_H3 : constant DB.Utils.Hash_Type := Hash_3 (L);
      L_H4 : constant DB.Utils.Hash_Type := Hash_4 (L);
      Arr  : constant array (Positive range <>) of DB.Utils.Hash_Type :=
        (K_H1, K_H2, K_H3, K_H4, L_H1, L_H2, L_H3, L_H4);
   begin
      Put_Line ("Hash_1 (K) ="& K_H1'Img);
      Put_Line ("Hash_2 (K) ="& K_H2'Img);
      Put_Line ("Hash_3 (K) ="& K_H3'Img);
      Put_Line ("Hash_4 (K) ="& K_H4'Img);
      Put_Line ("Hash_1 (L) ="& L_H1'Img);
      Put_Line ("Hash_2 (L) ="& L_H2'Img);
      Put_Line ("Hash_3 (L) ="& L_H3'Img);
      Put_Line ("Hash_4 (L) ="& L_H4'Img);
      for I in Arr'Range loop
         for J in Arr'Range loop
            if I /= J then
               Assert (Arr (I) /= Arr (J), "Hashes "& I'Img &" and "&
                       J'Img &" are equal: "& Arr (I)'Img &" = "& Arr (J)'Img);
            end if;
         end loop;
      end loop;
   end Test_Hash;

end DB.Types.Strings_Bounded.Test;

