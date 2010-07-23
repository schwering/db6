-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.DSA.Map_Reduce)
procedure Gen_Map_Reduce
is

   ----------
   -- The map phase. It just calls the user-supplied subprogram Input until
   -- all In_Key/In_Value-pairs are consumed. The user-supplied Map procedure
   -- is called with each In_Key/In_Value-pair and an access to the Emit
   -- procedure.

   procedure Map_Phase
   is
      task type Map_Task_Type is
         entry Start;
      end Map_Task_Type;

      task body Map_Task_Type
      is
         procedure Emit
           (Key   : in Intermediate_Key_Type;
            Value : in Intermediate_Value_Type) is
         begin
            Intermediate_Output (Key, Value);
         end Emit;

         In_Key   : In_Key_Type;
         In_Value : In_Value_Type;
         Success  : Boolean;
      begin
         accept Start;
         loop
            Input (In_Key, In_Value, Success);
            exit when not Success;
            Map (In_Key, In_Value, Emit'Access);
         end loop;
      end Map_Task_Type;

      Map_Tasks : array (1 .. Map_Task_Count) of Map_Task_Type;
   begin
      for I in Map_Tasks'Range loop
         Map_Tasks (I).Start;
      end loop;
   end Map_Phase;


   ----------
   -- The sorting phase. This is obsolete at the moment because sorting is
   -- currently done at the time of inserting since we use a BTree as
   -- temporary storage.

   procedure Sort_Phase is
   begin
      Sort_Intermediate_Storage;
   end;


   ----------
   -- The reduce phase. There are two types tasks:
   -- 1. The first one traverses the intermediate key/value pairs and
   --    produces Key_Values_Type objects which consists of 1 key and
   --    some positive number of values that are associated with the key.
   --    Only one of these tasks exists.
   -- 2. The second one is the consumer task type. There might be multiple
   --    consumers (Reduce_Task_Count many). Each task chooses one
   --    Key_Values_Type; if there is none at the moment, it waits until it
   --    gets one. Then it consumes all the values in this Key + Value-
   --    sequence object and reduces them (by calling the user's Reduce
   --    subprogram).
   -- Just a note about the queues we use here: there is one queue that
   -- stores Key + Value-sequence objects. It is populated by the cursor
   -- task. The Value-sequences again are queues, but they live on the heap.
   -- While the cursor task creates these objects and populates them and also
   -- marks them as final (this is the case when there are no more values for
   -- the specific key), their memory is freed by that reduce task that
   -- consumed the Key + Value-sequence object.

   procedure Reduce_Phase
   is
      package Value_Queues is new Utils.Gen_Queues
         (Queue_Size => Value_Queue_Size,
          Item_Type  => Intermediate_Value_Type);

      type Key_Values_Type is
         record
            Key         : Intermediate_Key_Type;
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

      task body Cursor_Task_Type is
      begin
         accept Start;
         declare
            Key_Values : Key_Values_Ref_Type := null;
         begin
            loop
               declare
                  Key        : Intermediate_Key_Type;
                  Value      : Intermediate_Value_Type;
                  Successful : Boolean;
               begin
                  Intermediate_Input (Key, Value, Successful);
                  -- Mark as final.
                  if Key_Values /= null and then
                    (not Successful or else Key_Values.Key /= Key) then
                     Value_Queues.Mark_Final (Key_Values.Value_Queue);
                  end if;
                  -- Leave loop.
                  exit when not Successful;
                  -- Possibly create a new Key + Value-sequence queue
                  if Key_Values = null or else Key_Values.Key /= Key then
                     Key_Values := new Key_Values_Type'(Key    => Key,
                                                        others => <>);
                     Queues.Enqueue (Queue, Key_Values);
                  end if;
                  -- What we really wanted: enqueue the value in the
                  -- Key + Value-sequence!
                  Value_Queues.Enqueue (Key_Values.Value_Queue, Value);
               end;
            end loop;
         end;
         Queues.Mark_Final (Queue);
      exception
         when others =>
            Queues.Mark_Final (Queue);
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
               Queues.Dequeue (Queue, Success, Key_Values);
               exit when not Success;
               declare
                  procedure Next_Value
                    (Value   : out Intermediate_Value_Type;
                     Success : out Boolean) is
                  begin
                     Value_Queues.Dequeue (Key_Values.Value_Queue, Success,
                                           Value);
                  end Next_Value;

                  Out_Key   : Out_Key_Type;
                  Out_Value : Out_Value_Type;
               begin
                  Reduce (Key_Values.Key, Next_Value'Access, Out_Key,
                          Out_Value);
                  Output (Out_Key, Out_Value);
               end;
               Free (Key_Values);
            end;
         end loop;
      end Reduce_Task_Type;

      Cursor_Task  : Cursor_Task_Type;
      Reduce_Tasks : array (1 .. Reduce_Task_Count) of Reduce_Task_Type;
   begin
      Cursor_Task.Start;
      for I in Reduce_Tasks'Range loop
         Reduce_Tasks (I).Start;
      end loop;
   end Reduce_Phase;

begin
   Map_Phase;
   Sort_Phase;
   Reduce_Phase;
end Gen_Map_Reduce;

