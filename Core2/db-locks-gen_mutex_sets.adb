-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Locks.Gen_Mutex_Sets is

   procedure Lock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type) is
   begin
      MS.Lock (Item);
   end Lock;


   procedure Try_Lock
     (MS      : in out Mutex_Set_Type;
      Item    : in     Item_Type;
      Success :    out Boolean) is
   begin
      MS.Try_Lock (Item, Success);
   end Try_Lock;


   procedure Unlock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type) is
   begin
      MS.Unlock (Item);
   end Unlock;


   protected body Mutex_Set_Type is
      procedure Try_Lock (Item : in Item_Type; Success : out Boolean)
      is
         Cursor : Sets.Cursor;
      begin
         Sets.Insert (Set, Item, Cursor, Success);
      end Try_Lock;

      entry Lock (Item : in Item_Type) when Reset'Count = 0
      is
         Cursor   : Sets.Cursor;
         Inserted : Boolean;
      begin
         Sets.Insert (Set, Item, Cursor, Inserted);
         if not Inserted then
            requeue Wait_Lock;
         end if;
      end Lock;

      entry Wait_Lock (Item : in Item_Type) when Removed
      is
         Cursor   : Sets.Cursor;
         Inserted : Boolean;
      begin
         Sets.Insert (Set, Item, Cursor, Inserted);
         if not Inserted then
            requeue Lock;
         end if;
      end Wait_Lock;

      entry Unlock (Item : in Item_Type) when True is
      begin
         pragma Assert (Sets.Contains (Set, Item));
         Sets.Exclude (Set, Item);
         if Wait_Lock'Count > 0 then
            Removed := True;
            requeue Reset;
         end if;
      end Unlock;

      entry Reset when Wait_Lock'Count = 0 is
      begin
         Removed := False;
      end Reset;
   end Mutex_Set_Type;

end DB.Locks.Gen_Mutex_Sets;

