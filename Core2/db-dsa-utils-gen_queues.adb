-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.DSA.Utils.Gen_Queues is

   function Succ (I : Index_Type) return Index_Type is
   begin
      if I = Index_Type'Last then
         return Index_Type'First;
      else
         return I + 1;
      end if;
   end Succ;


   procedure Enqueue (Q : in out Queue_Type; Item : in Item_Type) is
   begin
      Q.Enqueue(Item);
   end Enqueue;


   procedure Dequeue 
     (Q       : in out Queue_Type;
      Success :    out Boolean;
      Item    :    out Item_Type) is
   begin
      Q.Dequeue(Success, Item);
   end Dequeue;


   procedure Mark_Final (Q : in out Queue_Type) is
   begin
      Q.Mark_Final;
   end Mark_Final;


   function Is_Full (Q : Queue_Type) return Boolean is
   begin
      return Q.Is_Full;
   end Is_Full;


   function Is_Empty (Q : Queue_Type) return Boolean is
   begin
      return Q.Is_Empty;
   end Is_Empty;


   function Size (Q : Queue_Type) return Natural is
   begin
      return Q.Size;
   end Size;


   protected body Queue_Type is
      entry Enqueue (Item : in Item_Type) when not Is_Full is
      begin
         Arr(Tail) := Item;
         Tail      := Succ(Tail);
      end Enqueue;

      entry Dequeue (Success : out Boolean; Item : out Item_Type)
         when Final or not Is_Empty is
      begin
         if not Is_Empty then
            Item    := Arr(Head);
            Head    := Succ(Head);
            Success := True;
         else
            Success := False;
         end if;
      end Dequeue;

      procedure Mark_Final is
      begin
         Final := True;
      end Mark_Final;

      function Is_Full return Boolean is
      begin
         return Head = Succ(Tail);
      end Is_Full;

      function Is_Empty return Boolean is
      begin
         return Head = Tail;
      end Is_Empty;

      function Size return Natural is
      begin
         if Head <= Tail then
            return Natural(Tail) - Natural(Head);
         else
            return Natural(Arr'Last) - Natural(Head) + Natural(Tail);
         end if;
      end Size;
   end Queue_Type;

end DB.DSA.Utils.Gen_Queues;

