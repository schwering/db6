-- Abstract:
--
-- A set of bounded size of mutexes.
--
-- Design Notes;
--
-- TODO
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Containers.Ordered_Maps;
with DB.Locks.Mutexes;

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

   procedure Get_Is_Locked
     (MS        : in out Mutex_Set_Type;
      Item      : in     Item_Type;
      Is_Locked :    out Boolean);

private
   type Mutex_Ref_Type is access Mutexes.Mutex_Type;

   package Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Item_Type,
      Element_Type => Mutex_Ref_Type,
      "<"          => Gen_Mutex_Sets."<",
      "="          => Gen_Mutex_Sets."=");

   protected type Mutex_Set_Type is
      procedure Get_Lock (Item : in Item_Type; Mutex : out Mutex_Ref_Type);
   private
      Map : Maps.Map;
   end Mutex_Set_Type;

end DB.Locks.Gen_Mutex_Sets;

