-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Locks.Gen_Mutex_Sets is

   function Casted_Hash (Item : Item_Type) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type(Hash(Item));
   end Casted_Hash;


   procedure Lock
     (Mutexes : in out Mutex_Set_Type;
      Item    : in     Item_Type) is
   begin
      Mutexes.Lock(Item);
   end Lock;


   procedure Unlock
     (Mutexes : in out Mutex_Set_Type;
      Item    : in     Item_Type) is
   begin
      Mutexes.Unlock(Item);
   end Unlock;


   protected body Mutex_Set_Type is
      entry Lock (Item : Item_Type) when True is
      begin
         null;
      end Lock;

      procedure Unlock (Item : Item_Type) is
      begin
         null;
      end Unlock;
   end Mutex_Set_Type;

end DB.Locks.Gen_Mutex_Sets;

