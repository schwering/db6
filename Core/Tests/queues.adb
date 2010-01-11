with Ada.Text_IO; use Ada.Text_IO;
with DB.Utils.Gen_Queues;
with DB.Utils.Gen_Queues;

procedure Queues
is
   package Queues is new DB.Utils.Gen_Queues(4, Integer);
   use Queues;
   Q : Queue_Type;

   task Enqueue;
   task Dequeue;

   task body Enqueue
   is
      I : Integer := Integer'First + 1;
   begin
      loop
         Queues.Enqueue(Q, I);
         Put_Line("PUT   :"& I'Img & Queues.Size(Q)'Img &
                  " "& Queues.Is_Empty(Q)'Img & " "& Queues.Is_Full(Q)'Img);
         I := I + 1;
      end loop;
   end Enqueue;

   task body Dequeue
   is
      Success : Boolean;
      J, I : Integer;
   begin
      J := Integer'First;
      loop
         Queues.Dequeue(Q, Success, I);
         if not Success then
            Put_Line("Mark_Final'ed and empty ("& Queues.Is_Empty(Q)'Img &")");
            exit;
         end if;
         if I /= J + 1 then
            Put_Line("FEHLER:"& J'Img & I'Img & Queues.Size(Q)'Img &
                     " "& Queues.Is_Empty(Q)'Img & " "& Queues.Is_Full(Q)'Img);
         else
            Put_Line("OK    :"& J'Img & I'Img & Queues.Size(Q)'Img &
                     " "& Queues.Is_Empty(Q)'Img & " "& Queues.Is_Full(Q)'Img);
         end if;
         J := I;
         if J mod 100 = 0 and not Queues.Is_Empty(Q) then
            abort Enqueue;
            Queues.Mark_Final(Q);
            Put_Line("Mark_Final'ed ("& Queues.Is_Empty(Q)'Img &")");
         end if;
      end loop;
   end Dequeue;

begin
   null;
end Queues;

