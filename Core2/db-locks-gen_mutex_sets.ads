-- Abstract:
--
-- A set of bounded size of mutexes.
--
-- Design Notes;
--
-- TODO
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

