-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Locks.Gen_Mutex_Sets is

   function Rehash (Hash : Utils.Hash_Type) return Utils.Hash_Type is
   begin
      return Hash + 1;
   end Rehash;


   function Index (Hash : Utils.Hash_Type) return Utils.Hash_Type is
   begin
      return Hash mod Hashtable_Size;
   end Index;


   procedure Lock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type)
   is
      HH      : constant Utils.Hash_Type := Hash(Item);
      H       : Utils.Hash_Type := HH;
      Success : Boolean;
   begin
      loop
         exit when MS.Arr(Index(H)).Item = Item;
         H := Rehash(H);
         exit when H = HH;
      end loop;
      loop
         MS.Arr(Index(H)).Try_Lock(Item, Success);
         exit when Success;
         H := Rehash(H);
         exit when H = HH;
      end loop;
      if not Success then
         raise Lock_Error;
      end if;
   end Lock;


   procedure Unlock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type)
   is
      HH : constant Utils.Hash_Type := Hash(Item);
      H  : Utils.Hash_Type := HH;
   begin
      loop
         exit when MS.Arr(Index(H)).Item = Item;
         H := Rehash(H);
         exit when H = HH;
      end loop;
      if MS.Arr(Index(H)).Item = Item then
         MS.Arr(Index(H)).Unlock;
      else
         raise Lock_Error;
      end if;
   end Unlock;


   protected body Mutex_Type is
      entry Try_Lock (Item : in Item_Type; Success : out Boolean) when True is
      begin
         pragma Assert ((Current = Invalid_Item) = (not Locked));
         if Wait_For_Lock'Count = 0 and not Locked then
            Current := Item;
            Locked  := True;
            Success := True;
         elsif Current = Item then
            requeue Wait_For_Lock;
         else
            Success := False;
         end if;
      end Try_Lock;

      entry Wait_For_Lock (Item : in Item_Type; Success : out Boolean)
         when not Locked is
      begin
         pragma Assert (Current = Item);
         pragma Unreferenced (Item);
         Success := True;
         Locked  := True;
      end Wait_For_Lock;

      procedure Unlock is
      begin
         Current := Invalid_Item;
         Locked  := False;
      end Unlock;

      function Item return Item_Type is
      begin
         return Current;
      end Item;
   end Mutex_Type;

end DB.Locks.Gen_Mutex_Sets;

