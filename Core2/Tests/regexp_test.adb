with Ada.Text_IO; use Ada.Text_IO;
with DB.Utils.Regular_Expressions; use DB.Utils.Regular_Expressions;

procedure Regexp_Test
is
   S1  : constant String := "abbbbba";
   E1  : constant String := "abbbbba";
   RE1 : constant Regexp := Compile(E1);

   S2  : constant String := "aba";
   E2  : constant String := "a(b*)a";
   RE2 : constant Regexp := Compile(E2);

   S3  : constant String := "x";
   E3  : constant String := "x";
   RE3 : constant Regexp := Compile(E3);
begin
   Put_Line(S1&" match "&E1&" = "& Boolean'Image(Match(S1, RE1)));
   Put_Line(S2&" match "&E1&" = "& Boolean'Image(Match(S2, RE1)));
   Put_Line(S1&" match "&E2&" = "& Boolean'Image(Match(S1, RE2)));
   Put_Line(S2&" match "&E2&" = "& Boolean'Image(Match(S2, RE2)));
   New_Line;
   Put_Line(E1&" subset "&E1&" = "& Boolean'Image(Is_Subset(RE1, RE1)));
   Put_Line(E1&" subset "&E2&" = "& Boolean'Image(Is_Subset(RE1, RE2)));
   Put_Line(E2&" subset "&E2&" = "& Boolean'Image(Is_Subset(RE2, RE2)));
   Put_Line(E2&" subset "&E1&" = "& Boolean'Image(Is_Subset(RE2, RE1)));
   New_Line;
   Put_Line(E1&" cap "&E1&" = empty? "& Boolean'Image(Intersection_Is_Empty(RE1, RE1)));
   Put_Line(E1&" cap "&E2&" = empty? "& Boolean'Image(Intersection_Is_Empty(RE1, RE2)));
   Put_Line(E2&" cap "&E2&" = empty? "& Boolean'Image(Intersection_Is_Empty(RE2, RE2)));
   Put_Line(E2&" cap "&E1&" = empty? "& Boolean'Image(Intersection_Is_Empty(RE2, RE1)));
   Put_Line(E1&" cap "&E3&" = empty? "& Boolean'Image(Intersection_Is_Empty(RE1, RE3)));
   Put_Line(E2&" cap "&E3&" = empty? "& Boolean'Image(Intersection_Is_Empty(RE2, RE3)));
   New_Line;
   declare
      S12  : constant String := S1 &" | "& S2;
      RE12 : constant Regexp := Union (RE1, RE2);
   begin
      Put_Line(S12&" accepts "&S1&" = "& Boolean'Image(Match(S1, RE12)));
      Put_Line(S12&" accepts "&S2&" = "& Boolean'Image(Match(S2, RE12)));
      Put_Line(S12&" accepts "&S3&" = "& Boolean'Image(Match(S3, RE12)));
   end;
   New_Line;
   declare
      S13  : constant String := S1 &" | "& S3;
      RE13 : constant Regexp := Union (RE1, RE3);
   begin
      Put_Line(S13&" accepts "&S1&" = "& Boolean'Image(Match(S1, RE13)));
      Put_Line(S13&" accepts "&S2&" = "& Boolean'Image(Match(S2, RE13)));
      Put_Line(S13&" accepts "&S3&" = "& Boolean'Image(Match(S3, RE13)));
   end;
   New_Line;
   declare
      E23  : constant String := "("& E2 &")|("& E3 &")";
      RE23 : constant Regexp :=  Union (RE2, RE3); -- Compile (E23); -- Union (RE2, RE3);
   begin
      Put_Line(E23&" accepts "&S1&" = "& Boolean'Image(Match(S1, RE23)));
      Put_Line(E23&" accepts "&S2&" = "& Boolean'Image(Match(S2, RE23)));
      Put_Line(E23&" accepts "&S3&" = "& Boolean'Image(Match(S3, RE23)));
   end;
   New_Line;
   declare
      RE : constant Regexp :=  Union (Compile ("bla"), Compile ("blupp"));
   begin
      Put_Line("RE accepts bla = "& Boolean'Image(Match("bla", RE)));
      Put_Line("RE accepts blupp = "& Boolean'Image(Match("blupp", RE)));
      Put_Line("RE accepts blablupp = "& Boolean'Image(Match("blablupp", RE)));
      Put_Line("RE accepts bl = "& Boolean'Image(Match("bl", RE)));
   end;
end Regexp_Test;

