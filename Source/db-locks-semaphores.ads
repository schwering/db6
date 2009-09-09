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

