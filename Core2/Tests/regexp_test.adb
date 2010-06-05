with Ada.Text_IO; use Ada.Text_IO;
with DB.Utils.Regular_Expressions; use DB.Utils.Regular_Expressions;

procedure Regexp_Test
is
   S1 : constant String := "aaa";
   S2 : constant String := "aa*";
   RE1 : constant Regexp := Compile(S1);
   RE2 : constant Regexp := Compile(S2);
begin
   Put_Line("RE1.Match("&S1&") = "& Boolean'Image(Match(S1, RE1)));
   Put_Line("RE1.Match("&S1&") = "& Boolean'Image(Match(S2, RE1)));
   Put_Line("RE2.Match("&S1&") = "& Boolean'Image(Match(S1, RE1)));
   Put_Line("RE2.Match("&S2&") = "& Boolean'Image(Match(S2, RE1)));
   Put_Line("RE1 subset RE1 = "& Boolean'Image(Is_Subset(RE1, RE1)));
   Put_Line("RE1 subset RE2 = "& Boolean'Image(Is_Subset(RE1, RE2)));
   Put_Line("RE2 subset RE2 = "& Boolean'Image(Is_Subset(RE2, RE2)));
   Put_Line("RE2 subset RE1 = "& Boolean'Image(Is_Subset(RE2, RE1)));
end Regexp_Test;

