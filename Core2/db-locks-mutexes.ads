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

   procedure Try_Lock
     (M       : in out Mutex_Type;
      Success :    out Boolean);

   procedure Unlock
     (M : in out Mutex_Type);

private
   protected type Mutex_Type is
      entry Lock;
      procedure Try_Lock (Success : out Boolean);
      procedure Unlock;

   private
      Locked : Boolean := False;
   end Mutex_Type;

   pragma Inline (Lock);
   pragma Inline (Try_Lock);
   pragma Inline (Unlock);

end DB.Locks.Mutexes;

