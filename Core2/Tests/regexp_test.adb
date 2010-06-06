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
end Regexp_Test;

