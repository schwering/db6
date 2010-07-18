with Ada.Text_IO;
with DB.Blocks.Local_IO;
with DB.Blocks.Gen_ASCII_Layer;

procedure ASCII_Layer_Test
is
   package ASCII is new DB.Blocks.Gen_ASCII_Layer
     (DB.Blocks.Local_IO.IO_Signature);
   use ASCII;

   procedure Put_Line (S : String) renames Ada.Text_IO.Put_Line;

   subtype First_Range is Integer range 1 .. 10000;
   subtype Second_Range is Character range 'A' .. 'Z';

   File : File_Type;
begin
   Create_And_Open_Temporary ("blabla", File);

   Set_Mode (File, Write_Only);
   for I in First_Range loop
      for C in Second_Range loop
         Write (File, C);
      end loop;
   end loop;

   Set_Mode (File, Read_Only);
   for I in First_Range loop
      for C in Second_Range loop
         declare
            D   : Character;
            EOF : Boolean;
         begin
            Read (File, D, EOF);
            if EOF and (I /= First_Range'Last or C /= Second_Range'Last) then
                  Put_Line ("I ="& I'Img &" C = "& C);
            end if;
            if C /= D then
               Put_Line (C &" /= "& D);
            end if;
         end;
      end loop;
   end loop;
   declare
      D   : Character;
      EOF : Boolean;
   begin
      Read (File, D, EOF);
      if not EOF then
         Put_Line ("EOF: D = "& D);
      end if;
   end;

   Put_Line ("OK");
end ASCII_Layer_Test;

