-- Abstract:
--
-- A dynamically growing stack. Uses the standard heap.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Containers;
with Ada.Containers.Hashed_Sets;

with DB.Utils;

generic
   type Item_Type is private;
   with function Hash (Item : Item_Type) return Utils.Hash_Type;
   with function "=" (Left, Right : Item_Type) return Boolean is <>;
package DB.Locks.Gen_Mutex_Sets is
   pragma Preelaborate;

   type Mutex_Set_Type is limited private;

   procedure Lock
     (Mutexes : in out Mutex_Set_Type;
      Item    : in     Item_Type);

   procedure Unlock
     (Mutexes : in out Mutex_Set_Type;
      Item    : in     Item_Type);

private
   function Casted_Hash (Item : Item_Type) return Ada.Containers.Hash_Type;

   package HTs is new Ada.Containers.Hashed_Sets
     (Element_Type        => Item_Type,
      Hash                => Casted_Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

   protected type Mutex_Set_Type is
      entry Lock (Item : Item_Type);
      procedure Unlock (Item : Item_Type);

   private
      Table : HTs.Set;
   end Mutex_Set_Type;

   pragma Inline (Lock);
   pragma Inline (Unlock);

end DB.Locks.Gen_Mutex_Sets;

