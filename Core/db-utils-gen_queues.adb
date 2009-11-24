-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

package body DB.Utils.Gen_Queues is

   procedure Set_Final
     (Queue : in out Queue_Type) is
   begin
      Queue.Set_Final;
   end Set_Final;


   function Is_Final
     (Queue : Queue_Type)
      return Boolean is
   begin
      return Queue.Is_Final;
   end Is_Final;


   procedure Put
     (Queue : in out Queue_Type;
      Item  : in     Item_Type) is
   begin
      Queue.Put(Item);
   end Put;


   procedure Pop
     (Queue : in out Queue_Type;
      Item  :    out Item_Type;
      Final :    out Boolean) is
   begin
      Queue.Pop(Item, Final);
   end Pop;


   function "+" (L, R : Index_Type) return Index_Type is
   begin
      return Index_Type((Natural(L) + Natural(R)) mod Size);
   end "+";


   protected body Queue_Type is
      procedure Set_Final is
      begin
         Final := True;
      end Set_Final;


      function Is_Final return Boolean is
      begin
         return Final;
      end Is_Final;

      entry Put (Item : in Item_Type) when Head /= Tail + 1 is
      begin
         pragma Assert (not Final);
         Arr(Tail) := Item;
         Tail      := Tail + 1;
      end Put;

      entry Pop (Item : out Item_Type; Final : out Boolean)
         when Final or Head /= Tail is
      begin
         if not Queue_Type.Final then
            Item := Arr(Head);
            Head := Head + 1;
         end if;
         Final := Queue_Type.Final;
      end Pop;
   end Queue_Type;

end DB.Utils.Gen_Queues;

