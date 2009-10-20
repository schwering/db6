with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with System.Storage_Elements;
with DB.Compression.Deflate;

use Ada.Text_IO;
use DB.Compression.Deflate;

procedure Deflate
is
   package SSE renames System.Storage_Elements;

   type String_Type is array (Positive range <>) of Character;
   S : String_Type := "Eine Insel mit zwei Bergen in dem tiefen weiten Meer, "&
                      "ja der Jaques Chirac und seine Freunde haben wieder "&
                      "zugelangt. Das ist eine grosze dumme Sache. Mit vier "&
                      "Tunnels und einem Eisenbahnverkehr oder so aehnlich. "&
                      "Jedenfalls muss dieser Text noch ein biszchen laenger "&
                      "werden.";
   B : Buffer_Type(1 .. S'Length);
   for B'Address use S'Address;
begin
   Put_Line("S'Length ="& S'Length'Img);
   Put_Line("B'Length ="& B'Length'Img);
   declare
      use type Size_Type;
      Compr   : Buffer_Type
              := Deflate(B, B'Length + Worst_Deflate_Overhead(B'Length));
      S_Compr : String_Type(1 .. Compr'Length);
      for S_Compr'Address use Compr'Address;
   begin
      Put_Line("Worst Overhead ="& Worst_Deflate_Overhead(B'Length)'Img);
      Put_Line("Compr'Length ="& Compr'Length'Img);
      declare
         Uncompr   : Buffer_Type := Inflate(Compr, B'Length);
         S_Uncompr : String_Type(1 .. Uncompr'Length);
         for S_Uncompr'Address use Uncompr'Address;
      begin
         Put_Line("Uncompr'Length ="& Compr'Length'Img);
         Put_Line("Uncompr = "& String(S_Uncompr));
      end;
   end;
end Deflate;

