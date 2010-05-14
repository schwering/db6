-- Abstract:
--
-- Inserts a new Key/Value pair.
-- (1) The algorithm is as follows: first descent using the Scan_Node procedure,
--     hence at each level walk to the right until the node induces the subtree
--     that's ought to contain Key.
--     During the descent, no locks are held.
--     Stop when a leaf is read.
-- (2) Move to the right until the checked node is ought to contain Key. This
--     is exactly the Move_Right procedure which holds at most two locks at once
--     and exactly one lock at return.
--     Choose the node wisely: if duplicate keys are allowed and you always
--     choose the first node candidate, insertions after deletions will lead to
--     many splits. Hence, if the current leaf has no space for the entry and
--     the neighbor leaf could contain the entry, too, try to insert it in the
--     neighbor.
--     Now start the ascent:
-- (3.a) If the current node is not safe, split it. Update the left node's
--       high key in parent and insert the new node's high key plus address
--       into the parent.
--       The parent is found via Move_Right which holds at most two locks
--       at once. Additionally the current nodes are locked, hence at most four
--       locks are held. At the end, the parent node is locked (and becomes
--       the current node and the ascent is continued recursively).
-- (3.b) If the inserted key is the high key of the new node, update the high
--       key in the parent. The parent is found the same way as in case (3.a).
-- (3.c) Everything is fine.
-- The Build_Stack procedure locks the root sometimes. But this does not affect
-- the maximum count of simultaneous locks since at most two locks are held at
-- the time Build_Stack is called.
-- Hence the maximum count of simultaneously held locks is four.
--
-- Design Notes:
--
-- We have the handling procedures Write_And_Ascend, Update_High_Key, and
-- Insert_Key_And_Update_High_Key.
-- Write_And_Ascend terminates the handling of the current level. If necessary,
-- it ascends by calling Update_High_Key or Insert_Key_And_Update_High_Key on
-- the parent level.
--
-- Locking conventions (apply to all parameters of type Valid_Address_Type):
-- * In-parameters must be locked at entrance of the procedure by the caller.
-- * In-parameters must be unlocked by the called procedure.
-- * Out-parameters must be locked by the called procedure.
-- * The exception is Move_Right in that its in-parameter N_A must not be locked
--   (but the out-parameter N_A is locked).
--
-- Error conventions:
-- * Expect exceptions (IO_Error) from IO operations.
-- * Do not expect exceptions from any functions, including Nodes.Insertion
--   etc., as they indicate really serious problems in the tree structure or the
--   code.
-- * XXX Expect exceptions from Lock/Unlock or not? Or only from Lock but not
--   Unlock? Or success-parameter? Very difficult.
--
-- Stack building and guarantees:
-- * The stack always has Root_Node as last remaining element (except when it is
--   empty, of course).
-- * Build_Stack descends in the tree up to a given level.
-- * Move_Right thread-safely moves right at a given level. If a read node
--   appears to be from another level, then a split must have occurred. In fact,
--   the read node must have been at address Root_Address, because the node is
--   locked in advance and the address Root_Address is the only one whose
--   underlying node's level might change.
--   In this case we unlock the read node, i.e. Root_Address, immediately and
--   rebuild the stack. In the meantime and after the stack building, the root
--   might be split again and again, but this doesn't matter: The stack is
--   rebuilt and the top item in the stack will not be the root, because we
--   build the stack up to the expected level which is not the root level due to
--   the split.
--   Not earlier than in the next step in the ascend the root might be
--   reconsidered. If the level has changed since the stack was rebuilt, it is
--   rebuilt again and we are again below the root. This repeats until some time
--   the root wasn't split between the stack was built and the ascend has
--   reached the root.
-- * Pop_Inner's search for a node that has a pointer works because it is
--   guaranteed that the pointer to C_A has already been inserted. This is
--   because we use thread-safe Move_Right in the search in Pop_Inner.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.Gen_BTrees)
procedure Insert
  (Tree  : in out Tree_Type;
   Key   : in     Key_Type;
   Value : in     Value_Type;
   State :    out State_Type)
is
   pragma Assert (Tree.Initialized);

   Stack : Stacks.Stack_Type;

   use type Nodes.Valid_Index_Type;
   use type Blocks.Size_Type;
   N_A   : Nodes.Valid_Address_Type;
   N_Old : Nodes.RW_Node_Type;
begin
   if Key_Size_Bound(Key) > Max_Key_Size(Value_Size_Bound(Value)) then
      State := Failure;
      return;
   end if;

   Stacks.Initialize(Stack, Key);

   declare
      use type Nodes.Level_Type;

      function Exit_Cond (N : Nodes.Node_Type) return Boolean
      is
         function Fits_Into_Node (K : Key_Type; V : Value_Type) return Boolean
         is
            use type Nodes.Validation_State_Type;
            I : Nodes.Index_Type;
         begin
           I := Nodes.Key_Position(N, K);
           if not Nodes.Is_Valid(I) then
              I := Nodes.Degree(N) + 1;
           end if;
           return Nodes.Validation(Nodes.Insertion(N, I, K, V)) /=
                  Nodes.Too_Large;
         end Fits_Into_Node;
      begin
         if not Nodes.Is_Leaf(N) then
            return True;
         end if;
         if not Nodes.Is_Valid(Nodes.Link(N)) then
            return True;
         end if;
         case Compare(Key, Nodes.High_Key(N)) is
            when Utils.Less    => return True;
            when Utils.Equal   => return Fits_Into_Node(Key, Value);
            when Utils.Greater => return False;
         end case;
      end Exit_Cond;
   begin
      Stacks.Build_Stack(Tree, Stack, Nodes.Leaf_Level);
      Stacks.Pop(Stack, N_A);
      Stacks.Move_Right(Tree, Stack, Nodes.Leaf_Level, Exit_Cond'Access,
                        N_A, N_Old);
   end;

   declare
      N : Nodes.RW_Node_Type;
      I : Nodes.Index_Type;
   begin
      I := Nodes.Key_Position(N_Old, Key);
      if not Nodes.Is_Valid(I) then
         I := Nodes.Degree(N_Old) + 1;
      elsif not Allow_Duplicates and then Nodes.Key(N_Old, I) = Key then
         Stacks.Finalize(Stack);
         State := Failure;
         return;
      end if;
      N := Nodes.Insertion(N_Old, I, Key, Value);
      Stacks.Write_And_Ascend(Tree, Stack, N_A, N_Old, N);
      State := Success;
   end;

   Stacks.Finalize(Stack);

exception
   when others =>
      Stacks.Finalize(Stack);
      pragma Warnings (Off);
      State := Error;
      pragma Warnings (On);
      raise;
end Insert;

