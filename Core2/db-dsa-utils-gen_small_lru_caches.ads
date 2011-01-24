-- Abstract:
--
-- Generic Bloom filter implementation.
--
-- If Absent (Item) returns True, then Item is guaranteed to be absent. On the
-- other hand, if Absent (Item) returns False, Item might still be present.
--
-- If Contains (Item) returns True, the item may be absent (the reason is hash
-- collisions). If Contains (Item) returns False, this answer is fool-proof.
--
-- Design Notes:
--
-- No heap is used.
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Finalization;

with DB.Utils;

generic
   type Key_Type is private;
   type Value_Type is private;
   with function Hash (Key : Key_Type) return DB.Utils.Hash_Type is <>;
   with function "=" (A, B : Key_Type) return Boolean is <>;
   Slot_Count : in Positive;
package DB.DSA.Utils.Gen_Small_LRU_Caches is
   pragma Preelaborate;

   type Cache_Type is limited private;

   procedure Put
     (Cache : in out Cache_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type);

   procedure Get
     (Cache : in out Cache_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type;
      Found :    out Boolean);

private
   subtype Hash_Type is DB.Utils.Hash_Type;
   use type Hash_Type;

   package AF renames Ada.Finalization;

   type Node_Type;
   type Node_Ref_Type is access Node_Type;
   type Node_Type is
      record
         Key   : Key_Type;
         Value : Value_Type;
         Hash  : Hash_Type;
         Next  : Node_Ref_Type;
      end record;

   type Cache_Type is new AF.Limited_Controlled with
      record
         Head : Node_Ref_Type := null;
         Tail : Node_Ref_Type := null;
         Size : Natural := 0;
      end record;

   overriding
   procedure Initialize (Cache : in out Cache_Type);

   overriding
   procedure Finalize (Cache : in out Cache_Type);

end DB.DSA.Utils.Gen_Small_LRU_Caches;

