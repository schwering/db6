-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;

package body DB.Locks.Mutexes.Test is

   overriding
   procedure Set_Up (T : in out Test_Type) is
   begin
      null;
   end Set_Up;


   overriding
   procedure Tear_Down (T : in out Test_Type) is
   begin
      null;
   end Tear_Down;


   procedure Test_Try_Lock (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      Mutex   : Mutex_Type;
      Success : Boolean;
   begin
      Assert (not Is_Locked (Mutex), "Mutex is locked");

      Try_Lock (Mutex, Timeout => 1.0, Success => Success);
      Assert (Success, "Try_Lock didn't succeed");

      Assert (Is_Locked (Mutex), "Mutex is not locked");

      Try_Lock (Mutex, Timeout => 1.0, Success => Success);
      Assert (not Success, "Try_Lock didn't fail");

      declare
         task Unlocker;
         task body Unlocker is
         begin
            delay 0.2;
            Unlock (Mutex);
         end Unlocker;
      begin
         Try_Lock (Mutex, Timeout => 1.0, Success => Success);
         Assert (Success, "Try_Lock failed");
      end;
   end Test_Try_Lock;


   procedure Test_Lock (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      Mutex : Mutex_Type;
   begin
      Assert (not Is_Locked (Mutex), "Mutex is locked");

      select
         Mutex.Lock;
      or delay 0.5;
         Assert (False, "Lock did block");
      end select;

      Assert (Is_Locked (Mutex), "Mutex is not locked");

      select
         Mutex.Lock;
         Assert (False, "Lock didn't block");
      or delay 0.5;
      end select;

      declare
         task Unlocker;
         task body Unlocker is
         begin
            delay 0.2;
            Unlock (Mutex);
         end Unlocker;
      begin
         select
            Mutex.Lock;
         or delay 0.5;
            Assert (False, "Lock didn't unblock after Unlock");
         end select;
      end;

      Assert (Is_Locked (Mutex), "Mutex is not locked");

      Unlock (Mutex);

      Assert (not Is_Locked (Mutex), "Mutex is locked");
   end Test_Lock;

end DB.Locks.Mutexes.Test;

