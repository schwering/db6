-- Abstract:
--
-- see spec
--
-- Design Notes:
--
-- The core is the separate Gen_Map_Reduce.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

with DB.DSA.Gen_BTrees;
with DB.DSA.Utils.Gen_Comparisons;
with DB.DSA.Utils.Gen_Queues;

package body DB.DSA.Map_Reduce is

   procedure Gen_Map_Reduce
   is separate;


   procedure Gen_Local_Map_Reduce
     (Intermediates_File_Name : in String)
   is
      -- for some reasons, accessing Intermediate_*.*_Type directly raises
      -- compiler errors
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

      type Cursor_Ref_Type is access Intermediate_BTrees.Cursor_Type;
      procedure Free is new Ada.Unchecked_Deallocation
        (Intermediate_BTrees.Cursor_Type, Cursor_Ref_Type);

      Tree   : Intermediate_BTrees.Tree_Type;
      Cursor : Cursor_Ref_Type := null;

      procedure Sort_Intermediate_Storage is
         Neg_Inf : constant Intermediate_BTrees.Bound_Type :=
            Intermediate_BTrees.Negative_Infinity_Bound;
         Pos_Inf : constant Intermediate_BTrees.Bound_Type :=
            Intermediate_BTrees.Positive_Infinity_Bound;
      begin
         pragma Assert (Cursor = null);
         Cursor := new Intermediate_BTrees.Cursor_Type'(
            Intermediate_BTrees.New_Cursor
               (Tree        => Tree,
                Thread_Safe => False,
                Lower_Bound => Neg_Inf,
                Upper_Bound => Pos_Inf));
         pragma Assert (Cursor /= null);
      end Sort_Intermediate_Storage;

      procedure Intermediate_Output
        (Key   : in Intermediate_Keys_Type;
         Value : in Intermediate_Values_Type)
      is
         use type Intermediate_BTrees.State_Type;
         State : Intermediate_BTrees.State_Type;
      begin
         Intermediate_BTrees.Insert(Tree, Key, Value, State);
         if State /= Intermediate_BTrees.Success then
            raise Tree_Error;
         end if;
      end Intermediate_Output;

      procedure Intermediate_Input
        (Key     : out Intermediate_Keys_Type;
         Value   : out Intermediate_Values_Type;
         Success : out Boolean)
      is
         use type Intermediate_BTrees.State_Type;
         State : Intermediate_BTrees.State_Type;
      begin
         Intermediate_BTrees.Next(Tree, Cursor.all, Key, Value, State);
         Success := State = Intermediate_BTrees.Success;
      end Intermediate_Input;

      procedure Map_Reduce is new Gen_Map_Reduce
        (In_Key_Type               => In_Key_Type,
         In_Value_Type             => In_Value_Type,
         Input                     => Input,
         Intermediate_Key_Type     => Intermediate_Keys_Type,
         Intermediate_Value_Type   => Intermediate_Values_Type,
         "="                       => Intermediate_Keys."=",
         Sort_Intermediate_Storage => Sort_Intermediate_Storage,
         Intermediate_Input        => Intermediate_Input,
         Intermediate_Output       => Intermediate_Output,
         Map                       => Map,
         Out_Key_Type              => Out_Key_Type,
         Out_Value_Type            => Out_Value_Type,
         Reduce                    => Reduce,
         Output                    => Output,
         Map_Task_Count            => Map_Task_Count,
         Reduce_Task_Count         => Reduce_Task_Count,
         Value_Queue_Size          => Value_Queue_Size,
         Storage_Pool              => Storage_Pool);

   begin
      Intermediate_BTrees.Create_Temporary(Tree, Intermediates_File_Name);
      Map_Reduce;
      if Cursor /= null then
         Free(Cursor);
      end if;
      Intermediate_BTrees.Finalize(Tree);
   exception
      when others =>
         if Cursor /= null then
            Free(Cursor);
         end if;
         Intermediate_BTrees.Finalize(Tree);
         raise;
   end Gen_Local_Map_Reduce;

end DB.DSA.Map_Reduce;

