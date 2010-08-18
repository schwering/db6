-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;

package body DB.Utils.Regular_Expressions.Test is

   procedure Set_Up (T : in out Test_Type) is
   begin
      T.A_Plus  := new Regexp_Type'(Compile ("a+"));
      T.B_Plus  := new Regexp_Type'(Compile ("b+"));
      T.C_Plus  := new Regexp_Type'(Compile ("c+"));

      T.AB_Plus := new Regexp_Type'(Compile ("(ab)+"));
      T.BA_Plus := new Regexp_Type'(Compile ("(ba)+"));

      T.Regexps := (T.A_Plus, T.B_Plus, T.C_Plus, T.AB_Plus, T.BA_Plus);
   end;


   procedure Tear_Down (T : in out Test_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Regexp_Type, Regexp_Ref_Type);
   begin
      for I in T.Regexps'Range loop
         Free (T.Regexps (I));
      end loop;
   end;


   function Equal (L, R : Regexp_Type) return Boolean is
   begin
      --      a in L <=> b in R
      -- iff  a in L => b in R  and  b in B => a in L
      -- iff  L \ R = empty     and  R \ L = empty
      return Is_Empty (Difference (L => L, R => R)) and
             Is_Empty (Difference (L => R, R => L));
   end Equal;


   function Regexp (T : Test_Type; I : Natural) return Regexp_Type is
   begin
      return T.Regexps (I).all;
   end Regexp;


   procedure Test_Empty_Regexp (T : in out Test_Type) is
   begin
      Assert (Is_Empty (Union (Empty_Regexp, Empty_Regexp)),
              "Union of empty and empty is not empty");
      Assert (Is_Empty (Intersection (Empty_Regexp, Empty_Regexp)),
              "Intersectoin of empty and empty is not empty");
      Assert (Is_Empty (Difference (Empty_Regexp, Empty_Regexp)),
              "Difference of empty and empty is not empty");
      Assert (Is_Empty (Empty_Regexp),
              "Empty is not empty");
      Assert (Is_Subset (Empty_Regexp, Empty_Regexp),
              " Empty is not a subset of empty");

      for C in Character'Range loop
         declare
            S : constant String (1..1) := (others => C);
         begin
            Assert (not Match (S, Empty_Regexp),
                    "Empty accepts "& Integer'Image (Character'Pos (C)));
         end;
      end loop;

      for I in T.Regexps'Range loop
         Assert (Is_Subset (Empty_Regexp, T.Regexp (I)),
                 "Empty is not a subset of regexp "& I'Img);
         Assert (Is_Empty (Intersection (Empty_Regexp, T.Regexp (I))),
                 "Intersection of Empty and regexp "& I'Img &" is not empty");
         Assert (Equal (Union (Empty_Regexp, T.Regexp (I)), T.Regexp (I)),
                 "Union of empty and regexp "& I'Img &" is not equal to "&
                 "regexp "& I'Img);
      end loop;
   end;


   procedure Test_Union (T : in out Test_Type) is
      AB_Plus : Regexp_Type renames T.AB_Plus.all;
      BA_Plus : Regexp_Type renames T.BA_Plus.all;
   begin
      Assert (Is_Subset (AB_Plus, Union (AB_Plus, BA_Plus)),
              "(AB)+ \not\subseteq ((AB)+ \cup (BA)+)");
      Assert (Is_Subset (BA_Plus, Union (AB_Plus, BA_Plus)),
              "(BA)+ \not\subseteq ((AB)+ \cup (BA)+)");
      Assert (Is_Subset (Difference (Union (AB_Plus, BA_Plus), BA_Plus),
                         AB_Plus),
              "(AB)+  /=  (((AB)+ \cup (BA)+) \ (BA)+)");
      Assert (Is_Subset (Difference (Union (BA_Plus, AB_Plus), BA_Plus),
                         AB_Plus),
              "(AB)+  /=  (((BA)+ \cup (AB)+) \ (BA)+)");
      Assert (Is_Subset (Difference (Union (AB_Plus, BA_Plus), AB_Plus),
                         BA_Plus),
              "(BA)+  /=  (((AB)+ \cup (BA)+) \ (AB)+)");
      Assert (Is_Subset (Difference (Union (BA_Plus, AB_Plus), AB_Plus),
                         BA_Plus),
              "(BA)+  /=  (((BA)+ \cup (AB)+) \ (AB)+)");
      Assert (Equal (Union (AB_Plus, BA_Plus), Union (BA_Plus, AB_Plus)),
              "(AB)+ \cup (BA)+  /=  (BA)+ \cup (AB)+");
   end;


   procedure Test_Intersection (T : in out Test_Type) is
   begin
      for I in T.Regexps'Range loop
         for J in T.Regexps'Range loop
            Assert (Is_Subset (Intersection (T.Regexp (I), T.Regexp (J)),
                               T.Regexp (J)),
                    "("& I'Img &" \cap "& J'Img &") \not\subseteq "& J'Img);
            Assert (Is_Subset (Intersection (T.Regexp (I), T.Regexp (J)),
                               T.Regexp (I)),
                    "("& I'Img &" \cap "& J'Img &") \not\subseteq "& I'Img);
            Assert (Is_Subset (Empty_Regexp,
                               Intersection (T.Regexp (I), T.Regexp (J))),
                    "Empty \not\subseteq ("& I'Img &" \cap "& J'Img &")");
         end loop;
      end loop;
   end;


   procedure Test_Difference (T : in out Test_Type) is
   begin
      for I in T.Regexps'Range loop
         for J in T.Regexps'Range loop
            Assert (Is_Subset (Difference (T.Regexp (I), T.Regexp (J)),
                               T.Regexp (I)),
                    "("& I'Img &" \ "& J'Img &") \not\subseteq "& I'Img);
            Assert (Is_Subset (Empty_Regexp,
                               Difference (T.Regexp (I), T.Regexp (J))),
                    "Empty \not\subseteq ("& I'Img &" \ "& J'Img &")");
         end loop;
      end loop;
   end;


   procedure Test_Union2 (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      Rs : array (Positive range <>) of Regexp_Type :=
        (Compile ("((BMW|.*X3.*))|(.*Auto.*)|(.*wagen.*)"),
         Compile ("(Artificial Intelligence.*)|(.*Russel)|(.*Norvig)"),
         Compile ("(.*Kraft.*)|(.*Stoiber.*)"),
         Compile ("(.*Hogan.*)|(.*Newkirk.*)"),
         Compile ("(.*Klink.*)|(.*Schultz.*)"),
         Compile ("Halleluja"));
      R : Regexp_Type := Empty_Regexp;
   begin
      for J in Rs'Range loop
         R := Union (R, Rs (J));
         if J = 2 then
            Assert (Is_Subset (Compile ("Artificial Intelligence"),
                               Compile ("Artificial Intelligence.*")), "");
            Assert (Is_Subset (Compile ("Artificial Intelligence.*"),
                               Compile ("Artificial Intelligence.*")), "");
            Assert (Is_Subset (Compile ("Artificial Intelligence.*"), Rs (2)), "");
            Assert (Is_Subset (Compile ("Artificial Intelligence"), R), "");
            Assert (Is_Subset (Compile ("Russel"), R), "");
            Assert (Is_Subset (Compile ("Norvig"), R), "");
            Assert (Is_Subset (Compile (".*Russel"), R), "");
            Assert (Is_Subset (Compile (".*Norvig"), R), "");
            Assert (Is_Subset (Compile ("BMW"), R), "");
            Assert (Is_Subset (Compile (".*X3"), R), "");
            Assert (Is_Subset (Compile (".*Auto"), R), "");
            Assert (Is_Subset (Compile (".*wagen"), R), "");
            Assert (Is_Subset (Compile ("X3.*"), R), "");
            Assert (Is_Subset (Compile ("Auto.*"), R), "");
            Assert (Is_Subset (Compile ("wagen.*"), R), "");
            Assert (Is_Subset (Compile (".*X3.*"), R), "");
            Assert (Is_Subset (Compile (".*Auto.*"), R), "");
            Assert (Is_Subset (Compile (".*wagen.*"), R), "");
            Assert (Is_Subset (Compile ("Artificial IntelligenceABC"), R), "");
            Assert (Is_Subset (Compile ("Artificial Intelligence.*"), R), "");
         end if;
         Assert (Is_Subset (Rs (J), R),
                 "Regexp"& J'Img &" not a subset of union");
      end loop;
      for J in Rs'Range loop
         Assert (Is_Subset (Rs (J), R),
                 "Regexp"& J'Img &" not a subset of union");
      end loop;
   end Test_Union2;


   procedure Test_Long (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      I : Integer := 1500;
   begin
      while I <= 2000 loop
         I := I + 50;
         declare
            S : constant String (1 .. I) := (others => 'a');
         begin
            declare
               R : constant Regexp_Type := Compile (S);
               pragma Unreferenced (R);
            begin
               null;
            end;
         exception
            when E : others =>
               Put_Line (Exception_Information (E));
               Assert (False, "Caught exception at length "& I'Img);
               raise;
         end;
      end loop;
   end;

end DB.Utils.Regular_Expressions.Test;

