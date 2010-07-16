with Ada.Text_IO; use Ada.Text_IO;
with DB.Utils.Gen_Binary_Heaps;

procedure Heap_Test
is
   package Bin_Heaps is new DB.Utils.Gen_Binary_Heaps (Integer);
   use Bin_Heaps;

   Heap_Error : exception;

   From : constant := -1000;
   To   : constant := +1000;
   Heap : Heap_Type := New_Heap (To - From + 1);
begin
   for I in From .. -1 loop
      Insert (Heap, I);
   end loop;
   for I in reverse 1 .. To loop
      Insert (Heap, I);
   end loop;
   Insert (Heap, 0);

   for I in From .. To loop
      declare
         J : Integer;
      begin
         Extract_Min (Heap, J);
         if I /= J then
            Put_Line ("I ="& I'Img);
            Put_Line ("J ="& J'Img);
            raise Heap_Error;
         end if;
      end;
   end loop;
end Heap_Test;

