-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with DB.Locks.Semaphores; use DB.Locks.Semaphores;

procedure Locks
is
   L : Semaphore_Type;
   Ts : array (1..7) of Ticket_Type;
begin
   for I in Ts'Range loop
      Put_Line("Acqiring Ticket"& I'Img);
      Acquire_Ticket(L, Ts(I));
      Put_Line("Acqiring Ticket"& I'Img &" done");
   end loop;
   for I in Ts'Range loop
      Put_Line("Releasing Ticket"& I'Img & Ts(I)'Img);
      Release_Ticket(L, Ts(I));
      Put_Line("Releasing Ticket"& I'Img &" done");
   end loop;

   for I in Ts'Range loop
      Put_Line("Acqiring Ticket"& I'Img);
      Acquire_Ticket(L, Ts(I));
      Put_Line("Acqiring Ticket"& I'Img &" done");
   end loop;
   for I in Ts'Range loop
      Put_Line("Releasing Ticket"& I'Img & Ts(I)'Img);
      Release_Ticket(L, Ts(I));
      Put_Line("Releasing Ticket"& I'Img &" done");
   end loop;
end Locks;

