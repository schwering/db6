with Ada.Unchecked_Deallocation;

package body Util.Gen_Refcounters is

   function New_Object (Item : Item_Type) return Object_Type
   is begin
      return Object_Type'(Ada.Finalization.Controlled with
                          Item => Item, others => <>);
   end New_Object;


   overriding procedure Initialize (O : in out Object_Type)
   is begin
      O.Refcount := new Refcount_Type'(1);
   end Initialize;


   overriding procedure Adjust (O : in out Object_Type)
   is begin
      O.Refcount.all := O.Refcount.all + 1;
   end Adjust;


   overriding procedure Finalize (O : in out Object_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
         (Refcount_Type, Refcount_Ref_Type);
   begin
      if O.Refcount.all > 1 then
         O.Refcount.all := O.Refcount.all - 1;
      else
         Free(O.Refcount);
      end if;
   end Finalize;

end Util.Gen_Refcounters;

