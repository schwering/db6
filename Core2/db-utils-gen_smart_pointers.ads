-- Abstract:
--
-- A reference-counting pointer wrapper.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Finalization;

generic
   type Item_Type (<>) is limited private;
   type Item_Ref_Type is access Item_Type;
package DB.Utils.Gen_Smart_Pointers is

   type Smart_Pointer_Type is private;

   function New_Smart_Pointer (Ref : Item_Ref_Type) return Smart_Pointer_Type;
   function Ref (Pointer : Smart_Pointer_Type) return Item_Ref_Type;

private
   type Natural_Ref_Type is access Natural;

   type Smart_Pointer_Type is new Ada.Finalization.Controlled with
      record
         Ref       : Item_Ref_Type    := null;
         Ref_Count : Natural_Ref_Type;
      end record;

   overriding
   procedure Initialize
     (Ref : in out Smart_Pointer_Type);

   overriding
   procedure Adjust
     (Ref : in out Smart_Pointer_Type);

   overriding
   procedure Finalize
     (Ref : in out Smart_Pointer_Type);

end DB.Utils.Gen_Smart_Pointers;

