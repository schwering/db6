-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Locks.Gen_Mutex_Sets is

   procedure Lock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type)
   is
      Mutex : Mutex_Ref_Type;
   begin
      MS.Get_Lock(Item, Mutex);
      Mutexes.Lock(Mutex.all);
   end Lock;


   procedure Try_Lock
     (MS      : in out Mutex_Set_Type;
      Item    : in     Item_Type;
      Success :    out Boolean)
   is
      Mutex : Mutex_Ref_Type;
   begin
      MS.Get_Lock(Item, Mutex);
      Mutexes.Try_Lock(Mutex.all, Success);
   end Try_Lock;


   procedure Unlock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type)
   is
      Mutex : Mutex_Ref_Type;
   begin
      MS.Get_Lock(Item, Mutex);
      Mutexes.Unlock(Mutex.all);
   end Unlock;


   protected body Mutex_Set_Type is
      procedure Get_Lock (Item : in Item_Type; Mutex : out Mutex_Ref_Type) is
      begin
         if Maps.Contains(Map, Item) then
            Mutex := Maps.Element(Map, Item);
         else
            Mutex := new Mutexes.Mutex_Type;
            Maps.Insert(Map, Item, Mutex);
         end if;
      end Get_Lock;
   end Mutex_Set_Type;

end DB.Locks.Gen_Mutex_Sets;

