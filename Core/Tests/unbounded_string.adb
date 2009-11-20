-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO;

with DB.Types.Gen_Unbounded_Strings;

procedure Unbounded_String
is
   package Unbounded_String is new DB.Types.Gen_Unbounded_Strings
     (Item_Type => Character);
   use Unbounded_String;
   use Ada.Text_IO;
begin
   Put_Line("Declaration without initialization");
   declare
      S : String_Type;
   begin
      Put_Line("Declaration without initialization done");
      New_Line;
      Put_Line("Declaration with initialization");
      declare
         T : constant String_Type
           := New_String(Indefinite_Buffer_Type'("http://www.cnn.com"));
      begin
         Put_Line(String(To_String(T)));
         Put_Line("Declaration with initialization done");
         New_Line;
         Put_Line("Assignment");
         S := T;
         Put_Line(String(To_String(S)));
         Put_Line("Assignment done");
         New_Line;
         Put_Line("Finalization");
      end;
      Put_Line("Finalization done");
      New_Line;
      Put_Line("Finalization 2");
   end;
   Put_Line("Finalization 2 done");
   --Put_Line("RC ="& Natural'Image(RC));
   --Put_Line("BC ="& Natural'Image(BC));
end Unbounded_String;

