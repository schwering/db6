with Ada.Text_IO; use Ada.Text_IO;
with DB.Locks.Semaphores; use DB.Locks.Semaphores;

procedure Thread3
is
   Semaphore : Semaphore_Type;

   task type Thread (I : Integer);

   task body Thread
   is
      S      : constant String := Integer'Image(I);
      Ticket : Ticket_Type;
   begin
      Acquire_Ticket(Semaphore, Ticket);
      loop
         --Put_Line(S &": Read_Lock");
         --Read_Lock(Semaphore, Ticket);
         Put_Line(S &": Write_Lock");
         Write_Lock(Semaphore, Ticket);
         Put_Line(S &": Certify_Lock");
         Certify_Lock(Semaphore, Ticket);
         Put_Line(S &": Unlock");
         Unlock(Semaphore, Ticket);
      end loop;
      Release_Ticket(Semaphore, Ticket);
   end Thread;

   T1 : Thread(1);
   T2 : Thread(2);
begin
   null;
end Thread3;

