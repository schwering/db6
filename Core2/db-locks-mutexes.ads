-- Abstract:
--
-- Simple lock type.
--
-- Copyright 2008--2011 Christoph Schwering

package DB.Locks.Mutexes is
   pragma Pure;

   type Mutex_Type is limited private;

   procedure Lock
     (M : in out Mutex_Type);

   procedure Try_Lock
     (M       : in out Mutex_Type;
      Timeout : in     Duration := 0.0;
      Success :    out Boolean);

   procedure Unlock
     (M : in out Mutex_Type);

   function Is_Locked
     (M : Mutex_Type)
      return Boolean;

private
   protected type Mutex_Type is
      entry Lock;
      procedure Try_Lock  (Success : out Boolean);
      procedure Unlock;
      function Is_Locked return Boolean;

   private
      Locked : Boolean := False;
   end Mutex_Type;

   pragma Inline (Lock);
   pragma Inline (Try_Lock);
   pragma Inline (Unlock);

end DB.Locks.Mutexes;

