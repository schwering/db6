-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

package body DB.Locks.Mutexes is

   procedure Lock
     (M : in out Mutex_Type) is
   begin
      M.Lock;
   end Lock;


   procedure Unlock
     (M : in out Mutex_Type) is
   begin
      M.Unlock;
   end Unlock;


   protected body Mutex_Type is

      entry Lock when not Locked is
         begin
         Locked := True;
      end Lock;


      procedure Unlock is
      begin
         Locked := False;
      end Unlock;

   end Mutex_Type;

end DB.Locks.Mutexes;

