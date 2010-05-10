-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Locks.Mutexes is

   procedure Lock
     (M : in out Mutex_Type) is
   begin
      M.Lock(Ada.Task_Identification.Current_Task);
   end Lock;


   procedure Try_Lock
     (M       : in out Mutex_Type;
      Success :    out Boolean) is
   begin
      M.Try_Lock(Ada.Task_Identification.Current_Task, Success);
   end Try_Lock;


   procedure Unlock
     (M : in out Mutex_Type) is
   begin
      M.Unlock;
   end Unlock;


   function Is_Locked
     (M : Mutex_Type)
      return Boolean is
   begin
      return M.Is_Locked;
   end Is_Locked;


   function Owner
     (M : Mutex_Type)
      return ATI.Task_Id is
   begin
      return M.Owner;
   end Owner;


   protected body Mutex_Type is

      entry Lock (Owner : in ATI.Task_Id) when not Locked is
      begin
         Locked := True;
         Owning_Task := Owner;
      end Lock;


      procedure Try_Lock (Owner : in ATI.Task_Id; Success : out Boolean) is
      begin
         if Locked then
            Success := False;
         else
            Locked := True;
            Owning_Task := Owner;
            Success := True;
         end if;
      end Try_Lock;


      procedure Unlock is
      begin
         Locked := False;
         Owning_Task := ATI.Null_Task_Id;
      end Unlock;


      function Is_Locked return Boolean is
      begin
         return Locked;
      end Is_Locked;


      function Owner return ATI.Task_Id is
      begin
         return Owning_Task;
      end Owner;

   end Mutex_Type;

end DB.Locks.Mutexes;

