with Ada.Text_IO; use Ada.Text_IO;
with Ada.Task_Identification;
with Ada.Finalization;
with Ada.Exceptions;
with Ada.Exceptions.Traceback;

procedure Trans2
is 
   Column_Width : constant := 40;

   -- We use Task_Ids to recognize the different task.
   subtype Task_Id_Type is Ada.Task_Identification.Task_Id;
   use type Task_Id_Type;
   Null_Task_Id : Task_Id_Type renames Ada.Task_Identification.Null_Task_Id;

   function Current_Task_Id return Task_Id_Type
   renames Ada.Task_Identification.Current_Task;

   function Task_Name (Task_Id : Task_Id_Type) return String
   renames Ada.Task_Identification.Image;

   function Current_Task_Name return String
   is begin
      return Ada.Task_Identification.Image
         (Ada.Task_Identification.Current_Task);
   end Current_Task_Name;

   procedure Abort_Task (Task_Id : in Task_Id_Type)
   renames Ada.Task_Identification.Abort_Task;


   -- A lock type that stores information which task holds the lock.
   package Locks is
      protected type Lock_Type is
         entry Lock;
         procedure Unlock;
         entry Wait_For_Unlock;
         function Owning_Task_Id return Task_Id_Type;
      private
         Locked : Boolean      := False;
         Owner  : Task_Id_Type := Null_Task_Id;
      end Lock_Type;

      type Lock_Access_Type is access all Lock_Type;
   end Locks;

   package body Locks is
      protected body Lock_Type is
         entry Lock when not Locked
         is begin
            Locked := True;
            Owner  := Current_Task_Id;
         end Lock;

         procedure Unlock
         is begin
            Locked := False;
            Owner  := Null_Task_Id;
         end Unlock;

         entry Wait_For_Unlock when not Locked
         is begin
            null;
         end Wait_For_Unlock;

         function Owning_Task_Id return Task_Id_Type
         is begin
            return Owner;
         end Owning_Task_Id;
      end Lock_Type;
   end Locks;


   -- Deadlock-resolving task.
   task Deadlock_Resolver_Task is
      entry Set_Count (N : Natural);
      entry Add_Lock (Lock : Locks.Lock_Access_Type);
      entry Start;
   end Deadlock_Resolver_Task;

   task body Deadlock_Resolver_Task
   is
      Length : Natural := 0;
   begin
      accept Set_Count (N : in Natural) do
         Length := N;
      end Set_Count;
      declare
         Lock_Array : array (1 .. Length) of Locks.Lock_Access_Type;
      begin
         for I in Lock_Array'Range loop
            accept Add_Lock (Lock : in Locks.Lock_Access_Type) do
               Lock_Array(I) := Lock;
            end Add_Lock;
         end loop;
         accept Start;
         Put_Line("Ok, starting resolver");

         loop
            for I in Lock_Array'Range loop
               -- Observe the I-th lock for some time. If the lock is locked
               -- for all the time, abort the locks owner.
               -- The fact that some task owns a lock
               select
                  delay 5.0;
                  declare
                     Owning_Task_Id : Task_Id_Type
                                    renames Lock_Array(I).Owning_Task_Id;
                  begin
                     Put_Line("Aborting "& Task_Name(Owning_Task_Id));
                     Abort_Task(Owning_Task_Id);
                     for J in Lock_Array'Range loop
                        if Lock_Array(J).Owning_Task_Id = Owning_Task_Id then
                           Lock_Array(J).Unlock;
                        end if;
                     end loop;
                  end;
               then abort
                  Lock_Array(I).Wait_For_Unlock;
               end select;
            end loop;
         end loop;
      end;
   end Deadlock_Resolver_Task;


   -- Events. Currently, they are not used.
   -- I might use them for reporting that a transaction has been executed.
   generic
      type Item_Type is private;
   package Gen_Events is
      type Event_Type is limited private;

      Event_Error : exception;

      procedure Wait (Event : in out Event_Type; Item : out Item_Type);
      procedure Signal (Event : in out Event_Type; Item : in Item_Type);

   private
      protected type Event_Type is
         entry Wait (Item : out Item_Type);
         entry Signal (Item : in Item_Type);
      private
         entry Reset;
         Occured : Boolean := False;
         Item    : Item_Type;
      end Event_Type;
   end Gen_Events;

   package body Gen_Events is
      procedure Wait (Event : in out Event_Type; Item : out Item_Type)
      is begin
         Event.Wait(Item);
      end Wait;

      procedure Signal (Event : in out Event_Type; Item : in Item_Type)
      is begin
         Event.Signal(Item);
      end Signal;

      protected body Event_Type is
         entry Wait (Item : out Item_Type) when Occured
         is begin
            Item := Event_Type.Item;
         end Wait;

         entry Signal (Item : in Item_Type) when True
         is begin
            if Occured = True then
               raise Event_Error;
            end if;
            Occured := True;
            Event_Type.Item := Item;
            --requeue Reset;
         end Signal;

         entry Reset when Event_Type.Wait'Count = 0
         is begin
            Occured := False;
         end Reset;
      end Event_Type;
   end Gen_Events;


   -- A normal queue, used for transaction procedures later.
   generic
      type Item_Type is private;
      type Index_Type is mod <>;
   package Gen_Queues is
      type Queue_Type is limited private;
      procedure Enqueue (Queue : in out Queue_Type; Item : in Item_Type);
      procedure Dequeue (Queue : in out Queue_Type; Item : out Item_Type);

   private
      type Items_Type is array (Index_Type) of Item_Type;
      type Booleans_Type is array (Index_Type) of Boolean;

      protected type Queue_Type is
         entry Enqueue (Item : in Item_Type);
         entry Dequeue (Item : out Item_Type);
      private
         Items     : Items_Type;
         Used      : Booleans_Type := (others => False);
         Enq_Index : Index_Type := 0;
         Deq_Index : Index_Type := 0;
      end Queue_Type;
   end Gen_Queues;

   package body Gen_Queues is
      procedure Enqueue (Queue : in out Queue_Type; Item : in Item_Type)
      is begin
         Queue.Enqueue(Item);
      end Enqueue;

      procedure Dequeue (Queue : in out Queue_Type; Item : out Item_Type)
      is begin
         Queue.Dequeue(Item);
      end Dequeue;

      protected body Queue_Type is
         entry Enqueue (Item : in Item_Type) when not Used(Enq_Index)
         is begin
            Items(Enq_Index) := Item;
            Used(Enq_Index)  := True;
            Enq_Index        := Enq_Index + 1;
         end Enqueue;

         entry Dequeue (Item : out Item_Type) when Used(Deq_Index)
         is begin
            Item            := Items(Deq_Index);
            Used(Deq_Index) := False;
            Deq_Index       := Deq_Index + 1;
         end Dequeue;
      end Queue_Type;
   end Gen_Queues;


   -- Contains the transaction procedure access type and the queue.
   package Transactions is
      type Transaction_Proc_Type is access procedure;
      subtype Success_Type is Boolean;
      package Events is new Gen_Events(Success_Type);
      subtype Event_Type is Events.Event_Type;
      type Event_Access_Type is access all Event_Type;

      procedure Enqueue (T : in Transaction_Proc_Type;
                         E : in Event_Access_Type);
      procedure Dequeue (T : out Transaction_Proc_Type;
                         E : out Event_Access_Type);
   end Transactions;

   package body Transactions is
      type Transaction_Queue_Index is mod 8;
      type Item_Type is
         record
            T : Transaction_Proc_Type;
            E : Event_Access_Type;
         end record;
      package Transaction_Queues is new Gen_Queues
         (Item_Type, Transaction_Queue_Index);
      Ts : Transaction_Queues.Queue_Type;

      procedure Enqueue (T : in Transaction_Proc_Type;
                         E : in Event_Access_Type)
      is begin
         Transaction_Queues.Enqueue(Ts, (T, E));
      end Enqueue;

      procedure Dequeue (T : out Transaction_Proc_Type;
                         E : out Event_Access_Type)
      is
         I : Item_Type;
      begin
         Transaction_Queues.Dequeue(Ts, I);
         T := I.T;
         E := I.E;
      end Dequeue;
   end Transactions;


   -- The task supervisor executes a Transaction_Proc. If the execution is
   -- aborted, the Transaction_Proc is restarted as many times as needed
   -- until it completed.
   task type Transaction_Supervisor_Type;
   task body Transaction_Supervisor_Type
   is begin
      Put_Line("Transaction Supervisor "& Current_Task_Name);
      loop
         declare
            Transaction_Procedure : Transactions.Transaction_Proc_Type;
            Event_Access          : Transactions.Event_Access_Type;
            Completed             : Boolean := False;
         begin
            Transactions.Dequeue(Transaction_Procedure, Event_Access);
            <<Try_Again>>
            Put_Line("Executing");
            declare
               task Executor_Task;
               task body Executor_Task
               is begin
                  Transaction_Procedure.all;
                  Completed := True;
               exception
                  when Error : others =>
                     Put_Line("Exception in "&
                        Current_Task_Name &": "&
                        Ada.Exceptions.Exception_Information(Error));
               end Executor_Task;
            begin
               null;--abort Executor_Task;
            end;
            Put_Line("Completed = "& Completed'Img);
            if not Completed then
               goto Try_Again;
            end if;
            declare
               use type Transactions.Event_Access_Type;
            begin
               if Event_Access /= null then
                  Transactions.Events.Signal(Event_Access.all, Completed);
               end if;
            end;
            Put_Line("Executed");
         exception
            when Error : others =>
               Put_Line("Exception in "&
                  Current_Task_Name &": "&
                  Ada.Exceptions.Exception_Information(Error));
         end;
      end loop;
   end Transaction_Supervisor_Type;

   Supervisors : array (1 .. 2) of Transaction_Supervisor_Type;
   Lock_Array  : array (1 .. 2) of aliased Locks.Lock_Type;

   L : Locks.Lock_Type;

   procedure Put_Line (I : Positive; S : String)
   is begin
      L.Lock;
      declare
         Spaces : String(1 .. I*Column_Width) := (others => ' ');
      begin
         Put_Line(Spaces &"T"& I'Img &" ["& Current_Task_Name &"]: "& S);
      end;
      L.Unlock;
   end Put_Line;

   generic
      I : in Positive;
   procedure Transaction;
   procedure Transaction
   is begin
      if I mod 2 = 0 then
         Lock_Array(1).Lock;
         Put_Line(I, "having 1");
      else
         Lock_Array(2).Lock;
         Put_Line(I, "having 2");
      end if;
      Put_Line(I, "Okay, Houston, we've had a problem "&
                       "here");
      Put_Line(I, "This is Houston. Say again "&
                       "please.");
      if I mod 2 = 0 then
         Put_Line(I, "waiting for 2");
         Lock_Array(2).Lock;
         Put_Line(I, "got for 2");
      else
         Put_Line(I, "waiting for 1");
         Lock_Array(1).Lock;
         Put_Line(I, "got for 1");
      end if;
      Put_Line(I, "Houston, we've had a problem. We've had "&
                       "a main B bus undervolt.");
      Lock_Array(1).Unlock;
      Lock_Array(2).Unlock;
   end Transaction;

   procedure T1 is new Transaction(1);
   procedure T2 is new Transaction(2);
   procedure T3 is new Transaction(3);
   procedure T4 is new Transaction(4);
begin
   Deadlock_Resolver_Task.Set_Count(Lock_Array'Length);
   for I in Lock_Array'Range loop
      Deadlock_Resolver_Task.Add_Lock(Lock_Array(I)'Access);
   end loop;
   Deadlock_Resolver_Task.Start;

   Transactions.Enqueue(T1'Access, null);
   Transactions.Enqueue(T2'Access, null);

exception
   when Error : others =>
      Put_Line("Exception: "& Ada.Exceptions.Exception_Information(Error));
      Put_Line("Exception: "& Ada.Exceptions.Exception_Message(Error));
end Trans2;

