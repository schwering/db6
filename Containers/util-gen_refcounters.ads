with Ada.Finalization;

generic
   type Item_Type is private;
package Util.Gen_Refcounters is
   pragma Preelaborate;

   type Object_Type is new Ada.Finalization.Controlled with private;

   function New_Object (Item : Item_Type) return Object_Type;

private
   type Refcount_Type is new Natural;
   type Refcount_Ref_Type is access Refcount_Type;
   -- for Refcount_Ref_Type'Storage_Pool use Storage_Pool;

   type Object_Type is new Ada.Finalization.Controlled with
      record
         Item     : Item_Type;
         Refcount : Refcount_Ref_Type := null;
      end record;

   overriding procedure Initialize (O : in out Object_Type);
   overriding procedure Adjust (O : in out Object_Type);
   overriding procedure Finalize (O : in out Object_Type);

end Util.Gen_Refcounters;

