-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Locks.Mutexes is

   procedure Lock
     (M : in out Mutex_Type) is
   begin
      M.Lock;
   end Lock;


   procedure Try_Lock
     (M       : in out Mutex_Type;
      Success :    out Boolean) is
   begin
      M.Try_Lock(Success);
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


   protected body Mutex_Type is

      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;


      procedure Try_Lock (Success : out Boolean) is
      begin
         if Locked then
            Success := False;
         else
            Locked  := True;
            Success := True;
         end if;
      end Try_Lock;


      procedure Unlock is
      begin
         Locked := False;
      end Unlock;


      function Is_Locked return Boolean is
      begin
         return Locked;
      end Is_Locked;

   end Mutex_Type;

end DB.Locks.Mutexes;

