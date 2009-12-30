-- Abstract:
--
-- Advanced semaphores for read/write protection. These locks provide read-locks
-- for reading operations, write-locks for the isolated preparation of write
-- operations and certify-locks for making isolated changes durable.
--
-- An application that wants to lock some ressource first acquires a ticket.
-- This operation blocks if no more tickets are available. Tickets must be 
-- released when they are not needed anymore.
--
-- A ticket corresponds to a bank account. With each ticket, a number of permits
-- is associated. This number indicates how many permits are currently owned by
-- the ticket (the account balance).
-- An application that has been granted a ticket can apply for a lock (a
-- credit). There are three different kinds of locks: read-locks, write-locks
-- and certify-locks. No lock means 0 permits, read lock means 1 permit,
-- write lock means an absolute majority of permits, certify-lock means all
-- permits.
--
-- Tickets can upgrade their permit count by acquiring a respective lock.
-- Of course, upgrading procedures are blocking operations.
-- They can also downgrade by acquiring a less restrictive lock or even unlock.
-- Downgrading is non-blocking.
-- Note that before releasing a ticket, it must be unlocked.
--
-- A semaphore can be in the following states (all numbers are maxima, N is the
-- size of the ticket pool):
--     read-lock | write-lock | certify-lock
--         N     |      0     |        0
--       N/2-1   |      1     |        0
--         0     |      0     |        1
--
-- Note that this implementation does not prefer write-locks or certify-locks
-- over read-locks. See design notes.
--
-- References:
--
-- [Nagl] Manfred Nagl -- Softwaretechnik mit Ada 95
--
-- Design Notes:
--
-- The count of tickets is restricted to 8 at the moment. Hence the ticket pool
-- is pretty small. The reason is performance considerations, because the
-- ticket-usage-array is evaluated in many entry barriers.
--
-- To prefer write-locks or certify-locks, a task is needed [Nagl].
-- The following compromise might be a good idea: each downgrade operation could
-- check for waiting write-lock or certify-lock calls and donate the permits
-- to this application.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package DB.Locks.Semaphores is
   pragma Pure;

   type Ticket_Type is range 1 .. 8;
   type Semaphore_Type (Max_Permits : Positive := 8) is limited private;

   procedure Acquire_Ticket
      (S : in out Semaphore_Type;
       T :    out Ticket_Type);

   procedure Release_Ticket
      (S : in out Semaphore_Type;
       T : in     Ticket_Type);

   procedure Read_Lock
      (S : in out Semaphore_Type;
       T : in     Ticket_Type);

   procedure Write_Lock
      (S : in out Semaphore_Type;
       T : in     Ticket_Type);

   procedure Certify_Lock
      (S : in out Semaphore_Type;
       T : in     Ticket_Type);

   procedure Unlock
      (S : in out Semaphore_Type;
       T : in     Ticket_Type);

private
   type Bitset_Type is mod 2 ** (Natural(Ticket_Type'Last) -
                                 Natural(Ticket_Type'First));
   type Ticket_Size_Array_Type is array (Ticket_Type) of Natural;
   type Size_Type is (Shared, Majority, Exclusive);

   protected type Semaphore_Type (Max_Permits : Positive := 8) is
      entry Acquire_Ticket (T : out Ticket_Type);
      procedure Release_Ticket (T : in Ticket_Type);
      entry Read_Lock (Ticket_Type);
      entry Write_Lock (Ticket_Type);
      entry Certify_Lock (Ticket_Type);
      procedure Unlock (T : in Ticket_Type);

   private
      Count        : Natural := Max_Permits;
      Ticket_Used  : Bitset_Type            := 0;
      Ticket_Sizes : Ticket_Size_Array_Type := (others => 0);
   end Semaphore_Type;

   pragma Inline (Acquire_Ticket);
   pragma Inline (Release_Ticket);
   pragma Inline (Read_Lock);
   pragma Inline (Write_Lock);
   pragma Inline (Certify_Lock);
   pragma Inline (Unlock);

end DB.Locks.Semaphores;

