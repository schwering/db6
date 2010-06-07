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
end Regexp_Test;

