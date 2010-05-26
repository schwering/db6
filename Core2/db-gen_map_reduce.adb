-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;
with System.Storage_Pools;

with DB.Gen_BTrees;
with DB.Utils.Gen_Comparisons;
with DB.Utils.Gen_Queues;

procedure DB.Gen_Map_Reduce
  (Intermediates_File_Name : in String)
is

   ----------
   -- Helpers for intermediate keys/values.

   -- for some reasons, accessing Intermediate_*.*_Type directly raises compiler
   -- errors
   subtype Intermediate_Keys_Type is Intermediate_Keys.Key_Type;
   subtype Intermediate_Values_Type is Intermediate_Values.Value_Type;

   package Key_Comparisons is new Utils.Gen_Comparisons
     (Item_Type => Intermediate_Keys_Type,
      Compare   => Intermediate_Keys.Compare);
   use Key_Comparisons;

   package Intermediate_BTrees is new Gen_BTrees
     (Keys                     => Intermediate_Keys,
      Values                   => Intermediate_Values,
      Block_IO                 => Intermediate_Block_IO,
      Default_Allow_Duplicates => Allow_Intermediate_Duplicates);

   type Context_Type is
      record
         Intermediates : Intermediate_BTrees.Tree_Type;
      end record;


   ----------
   -- The map phase. It just calls the user-supplied subprogram Input until
   -- all In_Key/In_Value-pairs are consumed. The user-supplied Map procedure
   -- is called with each In_Key/In_Value-pair and an access to the Emit
   -- procedure.

   procedure Map_Phase
     (Context : in out Context_Type)
   is
      task type Map_Task_Type is
         entry Start;
      end Map_Task_Type;

      task body Map_Task_Type
      is
         procedure Emit
           (Key   : in Intermediate_Keys_Type;
            Value : in Intermediate_Values_Type)
         is
            use type Intermediate_BTrees.State_Type;
            State : Intermediate_BTrees.State_Type;
         begin
            Intermediate_BTrees.Insert(Context.Intermediates, Key, Value,
                                       State);
            if State /= Intermediate_BTrees.Success then
               raise Tree_Error;
            end if;
         end Emit;

         In_Key   : In_Key_Type;
         In_Value : In_Value_Type;
         Success  : Boolean;
      begin
         accept Start;
         loop
            Input(In_Key, In_Value, Success);
            exit when not Success;
            Map(In_Key, In_Value, Emit'Access);
         end loop;
      end Map_Task_Type;

      Map_Tasks : array (1 .. Map_Task_Count) of Map_Task_Type;
   begin
      for I in Map_Tasks'Range loop
         Map_Tasks(I).Start;
      end loop;
   end Map_Phase;


   ----------
   -- The sorting phase. This is obsolete at the moment because sorting is
   -- currently done at the time of inserting since we use a BTree as temporary
   -- storage.

   procedure Sort_Phase
     (Context : in out Context_Type)
   is null;


   ----------
   -- The reduce phase. There are two types tasks:
   -- 1. The first one traverses the intermediate key/value pairs and produces
   --    Key_Values_Type objects which consists of 1 key and some positive
   --    number of values that are associated with the key.
   --    Only one of these tasks exists.
   -- 2. The second one is the consumer task type. There might be multiple
   --    consumers (Reduce_Task_Count many). Each task chooses one
   --    Key_Values_Type; if there is none at the moment, it waits until it gets
   --    one. Then it consumes all the values in this Key + Value-sequence
   --    object and reduces them (by calling the user's Reduce subprogram).
   -- Just a note about the queues we use here: there is one queue that stores
   -- Key + Value-sequence objects. It is populated by the cursor task. The
   -- Value-sequences again are queues, but they live on the heap. While the
   -- cursor task creates these objects and populates them and also marks them
   -- as final (this is the case when there are no more values for the specific
   -- key), their memory is freed by that reduce task that consumed the Key +
   -- Value-sequence object.

   procedure Reduce_Phase
     (Context : in out Context_Type)
   is
      package Value_Queues is new Utils.Gen_Queues
         (Queue_Size => Value_Queue_Size,
          Item_Type  => Intermediate_Values_Type);

      type Key_Values_Type is
         record
            Key         : Intermediate_Keys_Type;
            Value_Queue : Value_Queues.Queue_Type;
         end record;

      type Key_Values_Ref_Type is access Key_Values_Type;
      for Key_Values_Ref_Type'Storage_Pool use Storage_Pool;

      package Queues is new Utils.Gen_Queues
         (Queue_Size => Reduce_Task_Count,
          Item_Type  => Key_Values_Ref_Type);

      Queue : Queues.Queue_Type;

      task type Cursor_Task_Type is
         entry Start;
      end Cursor_Task_Type;

      task body Cursor_Task_Type
      is
         Neg_Inf : constant Intermediate_BTrees.Bound_Type :=
            Intermediate_BTrees.Negative_Infinity_Bound;
         Pos_Inf : constant Intermediate_BTrees.Bound_Type :=
            Intermediate_BTrees.Positive_Infinity_Bound;
         Cursor  : Intermediate_BTrees.Cursor_Type :=
            Intermediate_BTrees.New_Cursor
               (Tree        => Context.Intermediates,
                Thread_Safe => False,
                Lower_Bound => Neg_Inf,
                Upper_Bound => Pos_Inf);
      begin
         accept Start;
         declare
            Key_Values : Key_Values_Ref_Type := null;
         begin
            loop
               declare
                  use type Intermediate_BTrees.State_Type;
                  Key     : Intermediate_Keys_Type;
                  Value   : Intermediate_Values_Type;
                  State   : Intermediate_BTrees.State_Type;
               begin
                  Intermediate_BTrees.Next(Context.Intermediates, Cursor,
                                           Key, Value, State);
                  -- Mark as final.
                  if Key_Values /= null and then
                     (State /= Intermediate_BTrees.Success or else
                     Key_Values.Key /= Key) then
                     Value_Queues.Mark_Final(Key_Values.Value_Queue);
                  end if;
                  -- Leave loop.
                  exit when State /= Intermediate_BTrees.Success;
                  -- Possibly create a new Key + Value-sequence queue
                  if Key_Values = null or else Key_Values.Key /= Key then
                     Key_Values := new Key_Values_Type'(Key    => Key,
                                                        others => <>);
                     Queues.Enqueue(Queue, Key_Values);
                  end if;
                  -- What we really wanted: enqueue the value in the
                  -- Key + Value-sequence!
                  Value_Queues.Enqueue(Key_Values.Value_Queue, Value);
               end;
            end loop;
         end;
         Intermediate_BTrees.Finalize_Cursor(Context.Intermediates, Cursor);
         Queues.Mark_Final(Queue);
      exception
         when others =>
            Intermediate_BTrees.Finalize_Cursor(Context.Intermediates, Cursor);
            Queues.Mark_Final(Queue);
            raise;
      end Cursor_Task_Type;


      task type Reduce_Task_Type is
         entry Start;
      end Reduce_Task_Type;

      task body Reduce_Task_Type is
      begin
         accept Start;
         loop
            declare
               procedure Free is new Ada.Unchecked_Deallocation
                 (Key_Values_Type, Key_Values_Ref_Type);

               Key_Values : Key_Values_Ref_Type;
               Success    : Boolean;
            begin
               Queues.Dequeue(Queue, Success, Key_Values);
               exit when not Success;
               declare
                  procedure Next_Value
                    (Value   : out Intermediate_Values_Type;
                     Success : out Boolean) is
                  begin
                     Value_Queues.Dequeue(Key_Values.Value_Queue, Success,
                                          Value);
                  end Next_Value;

                  Out_Key   : Out_Key_Type;
                  Out_Value : Out_Value_Type;
               begin
                  Reduce(Key_Values.Key, Next_Value'Access, Out_Key, Out_Value);
                  Output(Out_Key, Out_Value);
               end;
               Free(Key_Values);
            end;
         end loop;
      end Reduce_Task_Type;

      Cursor_Task  : Cursor_Task_Type;
      Reduce_Tasks : array (1 .. Reduce_Task_Count) of Reduce_Task_Type;
   begin
      Cursor_Task.Start;
      for I in Reduce_Tasks'Range loop
         Reduce_Tasks(I).Start;
      end loop;
   end Reduce_Phase;

   Context : Context_Type;
begin
   Intermediate_BTrees.Create(Intermediates_File_Name);
   Intermediate_BTrees.Initialize(Context.Intermediates,
                                  Intermediates_File_Name);
   Map_Phase(Context);
   Sort_Phase(Context);
   Reduce_Phase(Context);
   Intermediate_BTrees.Finalize(Context.Intermediates);
exception
   when others =>
      Intermediate_BTrees.Finalize(Context.Intermediates);
      raise;
end DB.Gen_Map_Reduce;

