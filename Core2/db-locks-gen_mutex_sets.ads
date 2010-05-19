-- Abstract:
--
-- A set of bounded size of mutexes.
--
-- Design Notes;
--
-- An item is locked iff it is contained in the set.
-- To lock an item, we try to insert it into the set. If this fails because the
-- set already contains the item, we have to wait until the item is unlocked.
-- Because waiting cannot be done that fine-grained, we wait until any item is
-- removed from the set, i.e. unlocked. This is done by requeuing a Wait_Lock.
-- The Unlock entry sets the flag Removed to True so that all enqueued
-- Wait_Locks can enter the entry and try to lock their item. If this fails --
-- and it will fail often -- it requeues a Lock call. Why not Wait_Lock? Because
-- this enqueued call could immediately enter Wait_Lock, but we want to wait
-- until the next item is unlocked. This is ensured by requeuing Lock and the
-- wait condition of Lock: Lock calls can only enter if there is no enqueued
-- Reset call. Reset's job is to set the Remove flag to False again and it is
-- called in the Unlock entry, i.e. before anyone can enter Wait_Lock.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Containers.Ordered_Sets;

generic
   type Item_Type is private;
   with function "<" (Left, Right : Item_Type) return Boolean is <>;
   with function "=" (Left, Right : Item_Type) return Boolean is <>;
package DB.Locks.Gen_Mutex_Sets is
   pragma Preelaborate;

   type Mutex_Set_Type is limited private;

   procedure Lock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type);

   procedure Try_Lock
     (MS      : in out Mutex_Set_Type;
      Item    : in     Item_Type;
      Success :    out Boolean);

   procedure Unlock
     (MS   : in out Mutex_Set_Type;
      Item : in     Item_Type);

private
   package Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Item_Type,
      "<"          => "<",
      "="          => "=");

   protected type Mutex_Set_Type is
      procedure Try_Lock (Item : in Item_Type; Success : out Boolean);
      entry Lock (Item : in Item_Type);
      entry Unlock (Item : in Item_Type);
   private
      entry Wait_Lock (Item : in Item_Type);
      entry Reset;
      Set     : Sets.Set;
      Removed : Boolean := False;
   end Mutex_Set_Type;

end DB.Locks.Gen_Mutex_Sets;

