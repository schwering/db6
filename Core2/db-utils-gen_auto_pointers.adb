-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Utils.Gen_Auto_Pointers is

   procedure Free is new Ada.Unchecked_Deallocation
     (Item_Type, Item_Ref_Type);


   function New_Auto_Pointer (Ref : Item_Ref_Type) return Auto_Pointer_Type is
   begin
      return Auto_Pointer_Type'(AF.Controlled with
                                Ptr => Ref);
   end New_Auto_Pointer;


   function New_Auto_Pointer (Item : Item_Type) return Auto_Pointer_Type is
   begin
      return Auto_Pointer_Type'(AF.Controlled with
                                Ptr => new Item_Type'(Item));
   end New_Auto_Pointer;


   procedure Initialize (Pointer : in out Auto_Pointer_Type) is
   begin
      Pointer.Ptr := null;
   end Initialize;


   procedure Adjust (Pointer : in out Auto_Pointer_Type) is
   begin
      if Pointer.Ptr /= null then
         Pointer.Ptr := new Item_Type'(Pointer.Ref.all);
      end if;
   end Adjust;


   procedure Finalize (Pointer : in out Auto_Pointer_Type) is
   begin
      if Pointer.Ptr /= null then
         Free (Pointer.Ptr);
      end if;
   end Finalize;


   function Ref (Pointer : Auto_Pointer_Type) return Item_Ref_Type is
   begin
      return Pointer.Ptr;
   end Ref;

end DB.Utils.Gen_Auto_Pointers;

