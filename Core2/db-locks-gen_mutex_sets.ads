-- Abstract:
--
-- A set of bounded size of mutexes. The locked items are of Item_Type.
-- Invalid_Item may never be locked (it's used to indicate an unused lock).
-- Be careful not to lock more than Hashtable_Size items at once! In this case
-- Lock_Error is thrown (but possibly deadlocks or infinite loops will occur,
-- I'll check that).
--
-- Design Notes;
--
-- The mutexes are held in a closed hashtable with Hashtable_Size entries. The
-- rehash-function just adds 1 for fast wrap-around.
--
-- The search for a lock is concurrent and therefore effective; mutual exclusion
-- happens only per-lock for each attempt to lock or unlock it.
--
-- The lock procedure first looks whether there is a lock for the specific item.
-- If it's found, another 
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils;

generic
   type Item_Type is private;
   with function "=" (Left, Right : Item_Type) return Boolean is <>;
   with function Hash (Item : Item_Type) return Utils.Hash_Type;
   Invalid_Item   : in Item_Type;
   Hashtable_Size : in Utils.Hash_Type;
package DB.Locks.Gen_Mutex_Sets is
   pragma Preelaborate;

   type Mutex_Set_Type is limited private;

   procedure Lock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type);

   procedure Unlock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type);

private
   protected type Mutex_Type is
      entry Try_Lock (Item : in Item_Type; Success : out Boolean);
      entry Wait_For_Lock (Item : in Item_Type; Success : out Boolean);
      procedure Unlock;
      function Item return Item_Type;
   private
      Current : Item_Type := Invalid_Item;
      Locked  : Boolean   := False;
   end Mutex_Type;

   use type Utils.Hash_Type;

   type Mutex_Array_Type is array (Utils.Hash_Type range 0 .. Hashtable_Size-1)
      of Mutex_Type;

   type Mutex_Set_Type is limited
      record
         Arr : Mutex_Array_Type;
      end record;

   pragma Inline (Lock);
   pragma Inline (Unlock);

end DB.Locks.Gen_Mutex_Sets;

