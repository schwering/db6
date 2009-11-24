-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

with Ada.Unchecked_Deallocation;

with DB.Utils.Gen_Queues;

package body DB.Gen_BTrees.Map_Reduce is

   ----------
   -- Random map reduce.

   procedure Gen_Random_Map_Reduce
     (Tree               : in out Tree_Type;
      Transaction        : in out Transaction_Type'Class;
      Cursor             : in out Cursor_Type;
      Element            :    out Element_Type;
      State              :    out Result_Type;
      Concurrency_Degree : in     Positive := Default_Concurrency_Degree)
   is
      task type Task_Type is
         entry Start;
         entry Is_Done (Element : out Element_Type; State : out Result_Type);
      end Task_Type;

      task body Task_Type
      is
         Element : Element_Type := Neutral_Element;
         Key     : Key_Type;
         Value   : Value_Type;
         State   : Result_Type;
      begin
         accept Start;
         loop
            Next(Tree, Transaction, Cursor, Key, Value, State);
            exit when State /= Success;
            Reduce(Element, Map(Key, Value));
         end loop;
         accept Is_Done (Element : out Element_Type; State : out Result_Type) do
            Element := Task_Type.Element;
            State   := Task_Type.State;
         end Is_Done;
      end Task_Type;

      Tasks : array (1 .. Concurrency_Degree) of Task_Type;
   begin
      Element := Neutral_Element;
      State   := Success;

      for I in Tasks'Range loop
         Tasks(I).Start;
      end loop;

      -- Tasks work now

      for I in Tasks'Range loop
         declare
            Step_Element : Element_Type;
            Step_State   : Result_Type;
         begin
            Tasks(I).Is_Done(Step_Element, Step_State);
            Reduce(Element, Step_Element);
            if Step_State /= Success then
               State := Step_State;
            end if;
         end;
      end loop;
   end Gen_Random_Map_Reduce;


   procedure Gen_Constrained_Random_Map_Reduce
     (Tree               : in out Tree_Type;
      Transaction        : in out Transaction_Type'Class;
      Cursor             : in out Cursor_Type;
      Element            :    out Element_Type;
      State              :    out Result_Type;
      Concurrency_Degree : in     Positive := Default_Concurrency_Degree)
   is
      task type Task_Type is
         entry Start (Index : in Positive);
         entry Is_Done (State : out Result_Type);
      end Task_Type;

      type Element_Access_Type is access Element_Type;

      procedure Free is new Ada.Unchecked_Deallocation
        (Element_Type, Element_Access_Type);

      Elements : array (1 .. Concurrency_Degree) of Element_Access_Type
               := (others => null);

      task body Task_Type
      is
         Index : Positive;
         Key   : Key_Type;
         Value : Value_Type;
         State : Result_Type;
      begin
         accept Start (Index : in Positive) do
            Task_Type.Index := Index;
         end Start;
         pragma Assert (Elements(Index) /= null);
         loop
            Next(Tree, Transaction, Cursor, Key, Value, State);
            exit when State /= Success;
            Reduce(Elements(Index).all, Map(Key, Value));
         end loop;
         accept Is_Done (State : out Result_Type) do
            State := Task_Type.State;
         end Is_Done;
      end Task_Type;

      Tasks : array (1 .. Concurrency_Degree) of Task_Type;
   begin
      Element := Neutral_Element;
      State   := Success;

      for I in Tasks'Range loop
         Elements(I) := new Element_Type'(Neutral_Element);
         Tasks(I).Start(I);
      end loop;

      -- Tasks work now

      for I in Tasks'Range loop
         declare
            Step_State : Result_Type;
         begin
            Tasks(I).Is_Done(Step_State);
            Reduce(Element, Elements(I).all);
            if Step_State /= Success then
               State := Step_State;
            end if;
         end;
      end loop;

      for I in Elements'Range loop
         Free(Elements(I));
      end loop;
   exception
      when others =>
         for I in Elements'Range loop
            Free(Elements(I));
         end loop;
         raise;
   end Gen_Constrained_Random_Map_Reduce;

   ----------
   -- Sequential map reduce.

   procedure Gen_Sequential_Map_Reduce
     (Tree               : in out Tree_Type;
      Transaction        : in out Transaction_Type'Class;
      Cursor             : in out Cursor_Type;
      Element            :    out Element_Type;
      State              :    out Result_Type;
      Concurrency_Degree : in     Positive := Default_Concurrency_Degree)
   is
      type Key_Value_Type is
         record
            Key   : Key_Type;
            Value : Value_Type;
         end record;

      package Queues is new Utils.Gen_Queues(Key_Value_Type, 32);

      Queue : Queues.Queue_Type;

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
            Queues.Put(Queue, KV);
         end loop;
         Queues.Set_Final(Queue);
         accept Is_Done (State : out Result_Type) do
            State := Producer_Type.State;
         end Is_Done;
      end Producer_Type;

      task type Consumer_Type is
         entry Start;
         entry Is_Done (Element : out Element_Type);
      end Consumer_Type;

      task body Consumer_Type
      is
         Element : Element_Type := Neutral_Element;
         KV      : Key_Value_Type;
         Final   : Boolean;
      begin
         accept Start;
         loop
            Queues.Pop(Queue, KV, Final);
            exit when Final;
            Reduce(Element, Map(KV.Key, KV.Value));
         end loop;
         accept Is_Done (Element : out Element_Type) do
            Element := Consumer_Type.Element;
         end Is_Done;
      end Consumer_Type;

      Producer  : Producer_Type;
      Consumers : array (1 .. Concurrency_Degree) of Consumer_Type;
   begin
      Element := Neutral_Element;
      State   := Success;

      Producer.Start;
      for I in Consumers'Range loop
         Consumers(I).Start;
      end loop;

      -- Tasks work now

      Producer.Is_Done(State);
      for I in Consumers'Range loop
         declare
            E : Element_Type;
         begin
            Consumers(I).Is_Done(E);
            Reduce(Element, E);
         end;
      end loop;
   end Gen_Sequential_Map_Reduce;


end DB.Gen_BTrees.Map_Reduce;

