-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

with Ada.Text_IO; use Ada.Text_IO;

procedure DB.Gen_BTrees.Gen_Sequential_Map_Reduce
  (Tree        : in out Tree_Type;
   Transaction : in out Transaction_Type'Class;
   Cursor      : in out Cursor_Type;
   Element     :    out Element_Type;
   State       :    out Result_Type)
is
   Concurrency_Degree : constant := 15;

   ----------
   -- A queue for the tasks. See below in task-coordination for its use.

   generic
      type Index_Type is mod <>;
      type Item_Type is (<>);
   package Gen_Consumer_Queue is
      type Array_Type is array (Index_Type) of Item_Type;

      protected Queue is
         function Top return Item_Type;
         entry Put (Item : in Item_Type);
         entry Pop_When_Equal (Item_Type);
         procedure Unlock;
      private
         Locked : Boolean    := False;
         Arr    : Array_Type;
         Head   : Index_Type := Index_Type'First;
         Tail   : Index_Type := Index_Type'First;
      end Queue;
   end Gen_Consumer_Queue;

   package body Gen_Consumer_Queue is
      protected body Queue is
         function Top return Item_Type is
         begin
            if Head = Tail then
               raise Constraint_Error;
            end if;
            return Arr(Head);
         end Top;

         entry Put (Item : in Item_Type) when not Locked and Head /= Tail + 1 is
         begin
            Arr(Tail) := Item;
            Tail      := Tail + 1;
            Locked    := True;
         end Put;

         entry Pop_When_Equal (for Item in Item_Type)
            when not Locked and (Head /= Tail and then Arr(Head) = Item) is
         begin
            --Item := Arr(Head);
            Head   := Head + 1;
            Locked := True;
         end Pop_When_Equal;

         procedure Unlock is
         begin
            Locked := False;
         end Unlock;
      end Queue;
   end Gen_Consumer_Queue;

   ----------
   -- A key for Key/Value pairs

   generic
      type Index_Type is mod <>;
      type Item_Type is private;
   package Gen_Key_Value_Queue is
      type Array_Type is array (Index_Type) of Item_Type;

      protected Queue is 
         procedure Set_Final;
         function Is_Final return Boolean;
         entry Put (Item : in Item_Type);
         entry Pop (Item : out Item_Type; Final : out Boolean);
      private
         Final : Boolean := False;
         Arr   : Array_Type;
         Head  : Index_Type := Index_Type'First;
         Tail  : Index_Type := Index_Type'First;
      end Queue;
   end Gen_Key_Value_Queue;

   package body Gen_Key_Value_Queue is
      protected body Queue is
         procedure Set_Final is
         begin
            Final := True;
         end Set_Final;

         function Is_Final return Boolean is
         begin
            return Final;
         end Is_Final;

         entry Put (Item : in Item_Type)
            when Head /= Tail + 1 is
         begin
            pragma Assert (not Final);
            Arr(Tail) := Item;
            Tail      := Tail + 1;
         end Put;

         entry Pop (Item : out Item_Type; Final : out Boolean)
            when Final or Head /= Tail is
         begin
            if not Queue.Final then
               Item := Arr(Head);
               Head := Head + 1;
            end if;
            Final := Queue.Final;
         end Pop;
      end Queue;
   end Gen_Key_Value_Queue;

   type Key_Value_Index_Type is mod 2**5;
   type Key_Value_Type is
      record
         Key   : Key_Type;
         Value : Value_Type;
      end record;
   package Key_Value_Queue is new Gen_Key_Value_Queue
     (Key_Value_Index_Type, Key_Value_Type);

   ----------
   -- Producer: reads from tree, puts into queue

   task type Producer_Type is
      entry Start;
      entry Is_Done (State : out Result_Type);
   end Producer_Type;

   task body Producer_Type
   is
      KV    : Key_Value_Type;
      State : Result_Type := Success;
   begin
      accept Start;
      loop
         Next(Tree, Transaction, Cursor, KV.Key, KV.Value, State);
         exit when State /= Success;
         Key_Value_Queue.Queue.Put(KV);
      end loop;
      Key_Value_Queue.Queue.Set_Final;
      accept Is_Done (State : out Result_Type) do
         State := Producer_Type.State;
      end Is_Done;
   end Producer_Type;

   ----------
   -- Consumer: pop from queue, map and then reduce

   type Consumer_ID_Type is mod Concurrency_Degree;

   task type Consumer_Type is
      entry Start (Consumer_ID : in Consumer_ID_Type);
      entry Is_Done;
   end Consumer_Type;

   type Consumer_Index_Type is mod Consumer_ID_Type'Modulus + 1;
   pragma Assert (Consumer_Index_Type'Modulus = 2**Consumer_Index_Type'Size);
   -- To have Consumer_ID_Type'Modulus slots in the queue, we need to have
   -- an index with Consumer_ID_Type'Modulus + 1 values.
   package Consumer_Queue is new Gen_Consumer_Queue
     (Consumer_Index_Type, Consumer_ID_Type);
   -- The consumer does three things: pop, map, reduce. Only map can be done
   -- parallel, because pop and reduce need to be performed in the correct
   -- order. The queue is used to control that if task X pops before Y, then
   -- X reduces before Y.

   task body Consumer_Type
   is
      Consumer_ID : Consumer_ID_Type;
      KV          : Key_Value_Type;
      Final       : Boolean;
   begin
      accept Start(Consumer_ID : in Consumer_ID_Type) do
         Consumer_Type.Consumer_ID := Consumer_ID;
      end Start;
      loop
         declare
         begin
            Consumer_Queue.Queue.Put(Consumer_ID);
            Key_Value_Queue.Queue.Pop(KV, Final);
            Consumer_Queue.Queue.Unlock;
            exit when Final;
            Consumer_Queue.Queue.Unlock;
         exception
            when others =>
               Consumer_Queue.Queue.Unlock;
               raise;
         end;
         declare
            Right : Element_Type := Map(KV.Key, KV.Value);
         begin
            Consumer_Queue.Queue.Pop_When_Equal(Consumer_ID);
            Reduce(Element, Right);
            Consumer_Queue.Queue.Unlock;
         exception
            when others =>
               Consumer_Queue.Queue.Unlock;
               raise;
         end;
      end loop;
      accept Is_Done;
   end Consumer_Type;


   Producer  : Producer_Type;
   Consumers : array (Consumer_ID_Type) of Consumer_Type;
begin
   Element := Neutral_Element;
   State   := Success;

   Producer.Start;
   for I in Consumers'Range loop
      Consumers(I).Start(I);
   end loop;

   -- Tasks work now

   Producer.Is_Done(State);
   for I in Consumers'Range loop
      Consumers(I).Is_Done;
   end loop;
end DB.Gen_BTrees.Gen_Sequential_Map_Reduce;

