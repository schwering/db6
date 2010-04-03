-- Abstract:
--
-- Simple lock type.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package DB.Locks.Mutexes is
   pragma Pure;

   type Mutex_Type is limited private;

   procedure Lock
     (M : in out Mutex_Type);

   procedure Unlock
     (M : in out Mutex_Type);

private
   protected type Mutex_Type is
      entry Lock;
      procedure Unlock;

   private
      Locked : Boolean := False;
   end Mutex_Type;

end DB.Locks.Mutexes;

