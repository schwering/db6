-- Abstract:
--
-- A controlled linked list.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Finalization;
with System.Storage_Pools;

generic
   type Item_Type is private;
   with function "=" (I, J : Item_Type) return Boolean;
   Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
package DB.Utils.Gen_Linked_Lists is

   type List_Type is limited private;
   type Iterator_Type is private;

   function Is_Empty
     (List : List_Type)
      return Boolean;

   function Size
     (List : List_Type)
      return Natural;

   function Deep_Copy
     (List : List_Type)
      return List_Type;

   procedure Prepend
     (List : in out List_Type;
      Item : in     Item_Type);

   procedure Append
     (List : in out List_Type;
      Item : in     Item_Type);

   procedure Pop_Head
     (List  : in out List_Type;
      Item  :    out Item_Type;
      Found :    out Boolean);

   procedure Pop_Tail
     (List  : in out List_Type;
      Item  :    out Item_Type;
      Found :    out Boolean);

   procedure Search
     (List    : in     List_Type;
      Matches : access function (Item : Item_Type) return Boolean;
      Item    :    out Item_Type;
      Found   :    out Boolean);

   procedure For_Each
     (List  : in List_Type;
      Visit : not null access procedure (Item : in out Item_Type));

   function Iterator
     (List : List_Type)
      return Iterator_Type;

   function Has_Next
     (Iterator : Iterator_Type)
      return Boolean;

   procedure Get_Next
     (Iterator : in out Iterator_Type;
      Item     :    out Item_Type);

   procedure Delete
     (Iterator : in out Iterator_Type);

private
   type Node_Type;
   type Node_Ref_Type is access Node_Type;
   for Node_Ref_Type'Storage_Pool use Storage_Pool;

   type Node_Type is
      record
         Item : Item_Type;
         Prev : Node_Ref_Type;
         Next : Node_Ref_Type;
      end record;

   type Modification_Counter_Type is mod 2**Integer'Size;
   type List_Ref_Type is not null access List_Type;
   for List_Ref_Type'Storage_Size use 0;

   type List_Type is new Ada.Finalization.Limited_Controlled with
      record
         Head : Node_Ref_Type             := null;
         Tail : Node_Ref_Type             := null;
         Self : List_Ref_Type             := List_Type'Unrestricted_Access;
         MCtr : Modification_Counter_Type := 0;
         Size : Natural                   := 0;
      end record;

   overriding
   procedure Finalize
     (List : in out List_Type);

   type Iterator_Type is
      record
         List    : List_Ref_Type;
         Curr    : Node_Ref_Type := null;
         MCtr    : Modification_Counter_Type;
         Deleted : Boolean := False;
      end record;

end DB.Utils.Gen_Linked_Lists;

