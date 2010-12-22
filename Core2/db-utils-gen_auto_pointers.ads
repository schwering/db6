-- Abstract:
--
-- A automatically copying and deallocating pointer wrapper.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Finalization;

generic
   type Item_Type (<>) is private;
   type Item_Ref_Type is access Item_Type;
package DB.Utils.Gen_Auto_Pointers is
   pragma Preelaborate;

   type Auto_Pointer_Type is tagged private;

   function New_Auto_Pointer (Item : Item_Type) return Auto_Pointer_Type;
   function Ref (Pointer : Auto_Pointer_Type) return Item_Ref_Type;

private
   package AF renames Ada.Finalization;

   type Auto_Pointer_Type is new AF.Controlled with
      record
         Ptr : Item_Ref_Type;
      end record;

   overriding
   procedure Initialize
     (Pointer : in out Auto_Pointer_Type);

   overriding
   procedure Adjust
     (Pointer : in out Auto_Pointer_Type);

   overriding
   procedure Finalize
     (Pointer : in out Auto_Pointer_Type);

end DB.Utils.Gen_Auto_Pointers;

