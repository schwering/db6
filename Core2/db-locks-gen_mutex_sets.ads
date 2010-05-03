-- Abstract:
--
-- A set of bounded size of mutexes.
--
-- Design Notes;
--
-- At the first level, a hashtable of fixed size is held. The values of the
-- table are sets that contain the currently locked addresses that have the
-- respective hash value. These sets are called buckets.
-- To ensure thread-safety, locks are needed somewhere in the (un)locking
-- process itself. The construction with a fixed-size hash table and sets as
-- the hashtable's values avoids top-level locking. Instead, it suffices to
-- ensure mutual exclusion at the bucket-level.
-- We do this with protected types. Unfortunately choosing the wait-conditions
-- is not that straightforward, because we cannot access the item that is to be
-- locked. If we could do this, the Lock-entry's wait-condition was simply
-- "when not Set.Contains(Item)".
-- Instead, the locking process is as follows: try to insert, i.e. lock, the
-- item directly. If this fails, requeue an entry call that waits until some
-- item was removed from the set, i.e. unlocked. When this happens.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Containers.Ordered_Sets;

with DB.Utils;

generic
   type Item_Type is private;
   with function "<" (Left, Right : Item_Type) return Boolean is <>;
   with function "=" (Left, Right : Item_Type) return Boolean is <>;
   with function Hash (Item : Item_Type) return Utils.Hash_Type;
   Hash_Range_Size : in Utils.Hash_Type;
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
   use type Utils.Hash_Type;

   package Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Item_Type,
      "<"          => "<",
      "="          => "=");

   protected type Bucket_Type is
      entry Insert (Item : in Item_Type);
      entry Remove (Item : in Item_Type);
   private
      entry Blocked_Insert (Item : in Item_Type);
      Set               : Sets.Set;
      Something_Removed : Boolean := False;
   end Bucket_Type;

   type Bucket_Array_Type is
      array (Utils.Hash_Type range 0 .. Hash_Range_Size - 1) of Bucket_Type;

   type Mutex_Set_Type is limited
      record
         Buckets : Bucket_Array_Type;
      end record;

end DB.Locks.Gen_Mutex_Sets;

