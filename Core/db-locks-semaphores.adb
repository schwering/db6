-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

package body DB.Locks.Semaphores is

   procedure Acquire_Ticket
      (S : in out Semaphore_Type;
       T :    out Ticket_Type) is
   begin
      S.Acquire_Ticket(T);
   end Acquire_Ticket;


   procedure Release_Ticket
      (S : in out Semaphore_Type;
       T : in      Ticket_Type) is
   begin
      S.Release_Ticket(T);
   end Release_Ticket;


   procedure Read_Lock
      (S : in out Semaphore_Type;
       T : in      Ticket_Type)
   is
      pragma Warnings (Off, T); -- crude compiler warning
   begin
      S.Read_Lock(T);
   end Read_Lock;


   procedure Write_Lock
      (S : in out Semaphore_Type;
       T : in     Ticket_Type)
   is
      pragma Warnings (Off, T); -- crude compiler warning
   begin
      S.Write_Lock(T);
   end Write_Lock;


   procedure Certify_Lock
      (S : in out Semaphore_Type;
       T : in     Ticket_Type)
   is
      pragma Warnings (Off, T); -- crude compiler warning
   begin
      S.Certify_Lock(T);
   end Certify_Lock;


   procedure Unlock
      (S : in out Semaphore_Type;
       T : in     Ticket_Type) is
   begin
      S.Unlock(T);
   end Unlock;


   protected body Semaphore_Type is

      function All_Tickets_Are_Used
         return Boolean is
      begin
        return Ticket_Used = Bitset_Type'Last;
     end All_Tickets_Are_Used;


      function Ticket_Is_Used
        (Ticket : Ticket_Type)
         return Boolean
      is
         pragma Inline (Ticket_Is_Used);
         I : constant Natural := Natural(Ticket - Ticket_Type'First);
      begin
         return (Ticket_Used and (2 ** I)) /= 0;
      end Ticket_Is_Used;


      procedure Set_Ticket_Used
        (Ticket : in Ticket_Type;
         Used   : in Boolean)
      is
         pragma Inline (Set_Ticket_Used);
         I : constant Natural := Natural(Ticket - Ticket_Type'First);
      begin
         if Used then
            Ticket_Used := Ticket_Used or (2 ** I);
         else
            Ticket_Used := Ticket_Used and (not (2 ** I));
         end if;
      end Set_Ticket_Used;


      entry Acquire_Ticket (T : out Ticket_Type)
         when not All_Tickets_Are_Used is
         begin
         for I in Ticket_Type'Range loop
            if not Ticket_Is_Used(I) then
               Set_Ticket_Used(I, True);
               Ticket_Sizes(I) := 0;
               T := I;
               return;
            end if;
         end loop;
         raise Lock_Error; -- never occurs due to guard
      end Acquire_Ticket;


      procedure Release_Ticket (T : in Ticket_Type) is
      begin
         if Ticket_Is_Used(T) and then Ticket_Sizes(T) /= 0 then
            raise Lock_Error;
         end if;
         Set_Ticket_Used(T, False);
      end Release_Ticket;


      function Size (S : Size_Type) return Positive
      is
         pragma Inline (Size);
      begin
         case S is
            when Shared =>    return 1;
            when Majority =>  return Max_Permits / 2 + 1;
            when Exclusive => return Max_Permits;
         end case;
      end Size;


      procedure Lock (T : Ticket_Type; S : Size_Type)
      is
         pragma Inline (Lock);
         Permits : constant Positive := Size(S);
      begin
         if not Ticket_Is_Used(T) then
            raise Lock_Error;
         end if;
         Count           := Count - Permits + Ticket_Sizes(T);
         Ticket_Sizes(T) := Permits;
      end Lock;


      entry Read_Lock (for T in Ticket_Type)
         when Count >= Size(Shared) - Ticket_Sizes(T) is
      begin
         Lock(T, Shared);
      end Read_Lock;


      entry Write_Lock (for T in Ticket_Type)
         when Count >= Size(Majority) - Ticket_Sizes(T) is
      begin
         Lock(T, Majority);
      end Write_Lock;


      entry Certify_Lock (for T in Ticket_Type)
         when Count >= Size(Exclusive) - Ticket_Sizes(T) is
      begin
         Lock(T, Exclusive);
      end Certify_Lock;


      procedure Unlock (T : Ticket_Type)
      is
         pragma Inline (Unlock);
      begin
         if not Ticket_Is_Used(T) then
            raise Lock_Error;
         end if;
         Count := Count + Ticket_Sizes(T);
         Ticket_Sizes(T) := 0;
      end Unlock;

   end Semaphore_Type;

end DB.Locks.Semaphores;

