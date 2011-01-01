-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.DSA.Utils.Gen_Queues is

   function Succ (I : Index_Type) return Index_Type is
   begin
      if I = Index_Type'Last then
         return Index_Type'First;
      else
         return I + 1;
      end if;
   end Succ;


   procedure Enqueue
     (Q    : in out Queue_Type;
      Item : in     Item_Type) is
   begin
      Q.Enqueue (Item);
   end Enqueue;


   procedure Enqueue
     (Q     : in out Queue_Type;
      Items : in     Item_Array_Type;
      Last  :    out Natural) is
   begin
      if Items'Length = 0 then
         Last := Items'Last;
      else
         Q.Enqueue (Items, Last);
      end if;
   end Enqueue;


   procedure Dequeue
     (Q       : in out Queue_Type;
      Item    :    out Item_Type;
      Success :    out Boolean) is
   begin
      Q.Dequeue (Item, Success);
   end Dequeue;


   procedure Dequeue
     (Q     : in out Queue_Type;
      Items :    out Item_Array_Type;
      Last  :    out Natural) is
   begin
      if Items'Length = 0 then
         Last := Items'Last;
      else
         Q.Dequeue (Items, Last);
      end if;
   end Dequeue;


   procedure Mark_Final (Q : in out Queue_Type) is
   begin
      Q.Mark_Final;
   end Mark_Final;


   function Is_Final (Q : Queue_Type) return Boolean is
   begin
      return Q.Is_Final;
   end Is_Final;


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
         Arr (Tail) := Item;
         Tail       := Succ (Tail);
      end Enqueue;


      entry Enqueue (Items : in Item_Array_Type; Last : out Natural)
         when not Is_Full
      is
         From   : Index_Type;
         Max_To : Index_Type;
         Count  : Natural;
         To     : Index_Type;
      begin
         if Head <= Tail then
            From   := Tail;
            Max_To := Arr'Last;
         else
            From   := Tail;
            Max_To := Head - 1;
         end if;
         Count := Natural'Min (Items'Length, Max_To - From + 1);
         To    := From + Count - 1;
         Last  := Items'First + Count - 1;
         Tail  := Succ (To);
         Arr (From .. To) := Items (Items'First .. Last);
      end Enqueue;


      entry Dequeue (Item : out Item_Type; Success : out Boolean)
         when Final or not Is_Empty is
      begin
         if not Is_Empty then
            Item    := Arr (Head);
            Head    := Succ (Head);
            Success := True;
         else
            Success := False;
         end if;
      end Dequeue;


      entry Dequeue (Items : out Item_Array_Type; Last : out Natural)
         when Final or not Is_Empty
      is
         From     : Index_Type;
         Max_To   : Index_Type;
         Count    : Natural;
         To       : Index_Type;
      begin
         if Head <= Tail then
            From   := Head;
            Max_To := Tail - 1;
         else
            From   := Head;
            Max_To := Arr'Last;
         end if;
         Count := Natural'Min (Items'Length, Max_To - From + 1);
         To    := From + Count - 1;
         Last  := Items'First + Count - 1;
         Head  := Succ (To);
         Items (Items'First .. Last) := Arr (From .. To);
      end Dequeue;


      procedure Mark_Final is
      begin
         Final := True;
      end Mark_Final;


      function Is_Final return Boolean is
      begin
         return Final and Is_Empty;
      end Is_final;


      function Is_Full return Boolean is
      begin
         return Head = Succ (Tail);
      end Is_Full;


      function Is_Empty return Boolean is
      begin
         return Head = Tail;
      end Is_Empty;


      function Size return Natural is
      begin
         if Head <= Tail then
            return Natural (Tail) - Natural (Head);
         else
            return Natural (Arr'Last) - Natural (Head) + Natural (Tail);
         end if;
      end Size;

   end Queue_Type;

end DB.DSA.Utils.Gen_Queues;

