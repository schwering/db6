-- Abstract:
--
-- A simple, non-distributed but concurrent implementation of Google's
-- MapReduce.
--
-- The user supplies In_Key/In_Value, an Intermediate_Key/Intermediate_Value and
-- Out_Key/Out_Value type pairs. For storage reasons, the serialization
-- procedures are needed for the intermediate objects.
--
-- For each In_Key/In_Value yielded by the Input subprogram, the user-supplied
-- Map procedure is called with the In_Key/In_Value objects and an access to an
-- Emit procedure as parameters. In his Map procedure, the user can analyze the
-- In_Key/In_Value objects, build some Intermediate_Key/Intermediate_Value-pairs
-- from it and call the Emit procedure for each of them. Then the MapReduce
-- implementation build (Intermediate_Key, [Intermediate_Value])-pairs (this is
-- Haskell-notation; there is one Intermediate_Key and a list of
-- Intermediate_Values). For each of these pairs, the users Reduce function is
-- called. Since the list of Intermediate_Values might be very long, it is not
-- given as parameter, but an access to a procedure called Next_Value which
-- is essentially a list iterator.
--
-- References:
--
-- J. Dean, S. Ghemawat -- MapReduce: Simplified Data Processing [...]
-- http://labs.google.com/papers/mapreduce.html
--
-- Design Notes:
--
-- See in-code comments.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Storage_Pools;

with DB.Blocks;
with DB.Blocks.Gen_IO_Signature;
with DB.Blocks.Gen_Keys_Signature;
with DB.Blocks.Gen_Values_Signature;

package DB.DSA.Map_Reduce is
   pragma Preelaborate;

   Default_Map_Task_Count    : constant := 10;
   Default_Reduce_Task_Count : constant := 10;
   Default_Value_Queue_Size  : constant := 100;

   generic
      type In_Key_Type is private;
      type In_Value_Type is private;
      with procedure Input
        (Key     : out In_Key_Type;
         Value   : out In_Value_Type;
         Success : out Boolean);

      type Intermediate_Key_Type is private;
      with function "=" (Left, Right : Intermediate_Key_Type) return Boolean;
      type Intermediate_Value_Type is private;
      with procedure Sort_Intermediate_Storage;
      with procedure Intermediate_Input
        (Key     : out Intermediate_Key_Type;
         Value   : out Intermediate_Value_Type;
         Success : out Boolean);
      with procedure Intermediate_Output
        (Key   : in Intermediate_Key_Type;
         Value : in Intermediate_Value_Type);

      with procedure Map
        (Key   : in     In_Key_Type;
         Value : in     In_Value_Type;
         Emit  : access procedure (Key : in Intermediate_Key_Type;
                                   Value : in Intermediate_Value_Type));

      type Out_Key_Type is private;
      type Out_Value_Type is private;
      with procedure Reduce
        (Key        : in     Intermediate_Key_Type;
         Next_Value : access procedure (Value : out Intermediate_Value_Type;
                                        Success : out Boolean);
         Out_Key    :    out Out_Key_Type;
         Out_Value  :    out Out_Value_Type);
      with procedure Output
        (Key   : in Out_Key_Type;
         Value : in Out_Value_Type);

      Map_Task_Count    : in Positive := Default_Map_Task_Count;
      Reduce_Task_Count : in Positive := Default_Reduce_Task_Count;
      Value_Queue_Size  : in Positive := Default_Value_Queue_Size;
      Storage_Pool      : in out System.Storage_Pools.Root_Storage_Pool'Class;
   procedure Gen_Map_Reduce;
   -- The raw map reduce procedure.
   -- The Sort_Intermediate_Storage parameter may be not thread-safe. No
   -- Intermediate_Input calls are made before, and all Intermediate_Output 
   -- are made before.

   generic
      type In_Key_Type is private;
      type In_Value_Type is private;
      with procedure Input
        (Key     : out In_Key_Type;
         Value   : out In_Value_Type;
         Success : out Boolean);

      with package Intermediate_Keys is new Blocks.Gen_Keys_Signature (<>);
      with package Intermediate_Values is new Blocks.Gen_Values_Signature (<>);
      with package Intermediate_Block_IO is new Blocks.Gen_IO_Signature (<>);
      Allow_Intermediate_Duplicates : in Boolean := True;

      with procedure Map
        (Key   : in     In_Key_Type;
         Value : in     In_Value_Type;
         Emit  : access procedure (Key : in Intermediate_Keys.Key_Type;
                                   Value : in Intermediate_Values.Value_Type));

      type Out_Key_Type is private;
      type Out_Value_Type is private;
      with procedure Reduce
        (Key        : in     Intermediate_Keys.Key_Type;
         Next_Value : access procedure
           (Value   : out Intermediate_Values.Value_Type;
            Success : out Boolean);
         Out_Key    :    out Out_Key_Type;
         Out_Value  :    out Out_Value_Type);
      with procedure Output
        (Key   : in Out_Key_Type;
         Value : in Out_Value_Type);

      Map_Task_Count    : in Positive := Default_Map_Task_Count;
      Reduce_Task_Count : in Positive := Default_Reduce_Task_Count;
      Value_Queue_Size  : in Positive := Default_Value_Queue_Size;
      Storage_Pool      : in out System.Storage_Pools.Root_Storage_Pool'Class;
   procedure Gen_Local_Map_Reduce
     (Intermediates_File_Name : in String);
   -- A non-distributed map reduce that stores the intermediate data in a
   -- temporary B-tree.

end DB.DSA.Map_Reduce;

