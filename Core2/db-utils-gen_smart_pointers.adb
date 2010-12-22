-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Utils.Gen_Smart_Pointers is

   function New_Smart_Pointer (Ref : Item_Ref_Type) return Smart_Pointer_Type is
   begin
      return Smart_Pointer_Type'(Ada.Finalization.Controlled with
                                 Ref       => Ref,
                                 Ref_Count => new Natural'(1));
   end New_Smart_Pointer;


   function Ref (Pointer : Smart_Pointer_Type) return Item_Ref_Type is
   begin
      return Pointer.Ref;
   end Ref;


   overriding
   procedure Initialize
     (Pointer : in out Smart_Pointer_Type) is
   begin
      Pointer.Ref       := null;
      Pointer.Ref_Count := new Natural'(1);
   end Initialize;


   overriding
   procedure Adjust
     (Pointer : in out Smart_Pointer_Type) is
   begin
      Pointer.Ref_Count.all := Pointer.Ref_Count.all + 1;
   end Adjust;


   overriding
   procedure Finalize
     (Pointer : in out Smart_Pointer_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Natural, Natural_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (Item_Type, Item_Ref_Type);
   begin
      Pointer.Ref_Count.all := Pointer.Ref_Count.all - 1;
      if Pointer.Ref_Count.all = 0 then
         Free (Pointer.Ref_Count);
         if Pointer.Ref /= null then
            Free (Pointer.Ref);
         end if;
      end if;
   end Finalize;

end DB.Utils.Gen_Smart_Pointers;

