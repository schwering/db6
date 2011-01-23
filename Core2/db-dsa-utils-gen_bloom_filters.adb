-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.DSA.Utils.Gen_Bloom_Filters is

   procedure Reset (Filter : in out Bloom_Filter_Type) is
   begin
      Filter.Mask := (others => False);
   end Reset;


   procedure Insert (Filter : in out Bloom_Filter_Type; Item : in Item_Type) is
   begin
      Filter.Mask (Hash_1 (Item) mod Size) := True;
      Filter.Mask (Hash_2 (Item) mod Size) := True;
      Filter.Mask (Hash_3 (Item) mod Size) := True;
      Filter.Mask (Hash_4 (Item) mod Size) := True;
   end Insert;


   function Contains
     (Filter : Bloom_Filter_Type; Item : Item_type)
      return Boolean is
   begin
      return Filter.Mask (Hash_1 (Item) mod Size) and then
             Filter.Mask (Hash_2 (Item) mod Size) and then
             Filter.Mask (Hash_3 (Item) mod Size) and then
             Filter.Mask (Hash_4 (Item) mod Size);
   end Contains;


   function Absent
     (Filter : Bloom_Filter_Type; Item : Item_type)
      return Boolean is
   begin
      return not Contains (Filter, Item);
   end Absent;

end DB.DSA.Utils.Gen_Bloom_Filters;

