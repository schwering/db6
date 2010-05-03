-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Locks.Gen_Mutex_Sets is

   function Index (Hash : Utils.Hash_Type) return Utils.Hash_Type
   is
      pragma Inline (Index);
   begin
      return Hash mod Hash_Range_Size;
   end Index;


   procedure Lock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type) is
   begin
      MS.Buckets(Index(Hash(Item))).Insert(Item);
   end Lock;


   procedure Unlock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type) is
   begin
      MS.Buckets(Index(Hash(Item))).Insert(Item);
   end Unlock;


   protected body Bucket_Type is
      entry Insert (Item : in Item_Type) when True
      is
         Cursor   : Sets.Cursor;
         Inserted : Boolean;
      begin
         Sets.Insert(Set, Item, Cursor, Inserted);
         if not Inserted then
            Something_Removed := False;
            requeue Blocked_Insert;
         end if;
      end Insert;

      entry Blocked_Insert (Item : in Item_Type)
         when Something_Removed and Insert'Count = 0 and Remove'Count = 0
      is
         Cursor   : Sets.Cursor;
         Inserted : Boolean;
      begin
         Sets.Insert(Set, Item, Cursor, Inserted);
         if not Inserted then
            requeue Insert;
         end if;
      end Blocked_Insert;

      entry Remove (Item : in Item_Type) when True is
      begin
         Sets.Exclude(Set, Item);
         Something_Removed := True;
      end Remove;
   end Bucket_Type;

end DB.Locks.Gen_Mutex_Sets;

