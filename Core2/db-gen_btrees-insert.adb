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

with DB.Utils.Gen_Stacks;
with DB.Utils.Global_Pool;

separate (DB.Gen_BTrees)
procedure Insert
  (Tree  : in out Tree_Type;
   Key   : in     Key_Type;
   Value : in     Value_Type;
   State :    out State_Type)
is
   pragma Assert (Tree.Initialized);

   package Stack is
      use Nodes;

      procedure Initialize;
      procedure Finalize;
      procedure Clear;
      function Is_Empty return Boolean;
      procedure Push (N_A : in Valid_Address_Type; Level : in Level_Type);
      procedure Pop (N_A : out Valid_Address_Type);
      procedure Pop (N_A : out Valid_Address_Type; Level : out Level_Type);

   private
      type Item_Type is
         record
            Address : Valid_Address_Type;
            Level   : Level_Type;
         end record;

      package Stacks is new Utils.Gen_Stacks
        (Item_Type    => Item_Type,
         Initial_Size => 7,
         Storage_Pool => Utils.Global_Pool.Global'Storage_Pool);

      Stack : Stacks.Stack_Type;
   end Stack;


   package body Stack is
      procedure Initialize is
      begin
         Stack := Stacks.New_Stack;
      end Initialize;

      procedure Finalize is
      begin
         Stacks.Finalize(Stack);
      end Finalize;

      procedure Clear is
      begin
         Stacks.Clear(Stack);
      end Clear;

      function Is_Empty return Boolean is
      begin
         return Stacks.Is_Empty(Stack);
      end Is_Empty;

      procedure Push (N_A : in Valid_Address_Type; Level : in Level_Type) is
      begin
         Stacks.Push(Stack, Item_Type'(N_A, Level));
      end Push;

      procedure Pop (N_A : out Valid_Address_Type)
      is
         Level : Level_Type;
      begin
         Pop(N_A, Level);
      end Pop;

      procedure Pop (N_A : out Valid_Address_Type; Level : out Level_Type)
      is
         Item : Item_Type;
      begin
         Stacks.Pop(Stack, Item);
         N_A := Item.Address;
         Level := Item.Level;
      end Pop;
   end Stack;


   procedure Build_Stack
     (Level : in Nodes.Level_Type)
   is
      use type Nodes.Level_Type;
      N_A : Nodes.Valid_Address_Type;
      N   : Nodes.RO_Node_Type;
   begin
      Stack.Clear;

      N_A := Root_Address;
      Read_Node(Tree, N_A, N);
      if Nodes.Is_Valid(Nodes.Link(N)) then
         -- Lock root to enforce that it is the first node in the stack.
         Lock(Tree, N_A);
         declare
         begin
            Read_Node(Tree, N_A, N);
            pragma Assert (not Nodes.Is_Valid(Nodes.Link(N)));
         exception
            when others =>
               Unlock(Tree, N_A);
               raise;
         end;
         Unlock(Tree, N_A);
      end if;

      loop
         if Nodes.Level(N) = Level then
            Stack.Push(N_A, Nodes.Level(N));
            exit;
         end if;
         pragma Assert (Nodes.Level(N) > Level);
         declare
            use type Nodes.Address_Type;
            NN_A : constant Nodes.Valid_Address_Type := Scan_Node(N, Key);
         begin
            if Nodes.To_Address(NN_A) /= Nodes.Link(N) then
               Stack.Push(N_A, Nodes.Level(N));
            end if;
            N_A := NN_A;
         end;
         Read_Node(Tree, N_A, N);
      end loop;
   end Build_Stack;


   procedure Move_Right
     (Level     : in              Nodes.Level_Type;
      Exit_Cond : not null access function (N : Nodes.Node_Type) return Boolean;
      N_A       : in out          Nodes.Valid_Address_Type;
      N         :    out          Nodes.Node_Type)
   is
      use type Nodes.Level_Type;
      use type Nodes.Valid_Address_Type;

      function Precise_Exit_Cond (N : Nodes.Node_Type) return Boolean is
      begin
         if Nodes.Level(N) /= Level then
            pragma Assert (Stack.Is_Empty);
            return True;
         end if;
         return Exit_Cond(N);
      end Precise_Exit_Cond;
   begin
      Move_Right(Tree, Precise_Exit_Cond'Access, N_A, N);
      if Nodes.Level(N) /= Level then
         pragma Assert (N_A = Root_Address);
         Unlock(Tree, N_A);
         Build_Stack(Level);
         Stack.Pop(N_A);
         pragma Assert (N_A /= Root_Address);
         Move_Right(Tree, Precise_Exit_Cond'Access, N_A, N);
         pragma Assert (Nodes.Level(N) = Level);
      end if;
   end Move_Right;


   procedure Pop_Inner
     (C_A : in  Nodes.Valid_Address_Type;
      N_A : out Nodes.Valid_Address_Type;
      N   : out Nodes.RW_Node_Type)
   is
      function Exit_Cond (N : Nodes.Node_Type) return Boolean is
      begin
         return Nodes.Is_Valid(Nodes.Child_Position(N, C_A));
      end Exit_Cond;

      Level : Nodes.Level_Type;
   begin
      declare
      begin
         Stack.Pop(N_A, Level);
         Move_Right(Level, Exit_Cond'Access, N_A, N);
      exception
         when others =>
            Unlock(Tree, C_A);
            raise;
      end;
      Unlock(Tree, C_A);
   end Pop_Inner;


   procedure Write_And_Ascend
     (N_A   : in Nodes.Valid_Address_Type;
      N_Old : in Nodes.RW_Node_Type;
      N     : in Nodes.RW_Node_Type);


   -- Handles the case that the high-key of a node C changed. Then C_A is the
   -- address of C and C_Key is the high-key of C.
   procedure Update_High_Key
     (C_Key : in Key_Type;
      C_A   : in Nodes.Valid_Address_Type) is
   begin
      if Stack.Is_Empty then
         Unlock(Tree, C_A);
         State := Success;
      else
         declare
            N_A   : Nodes.Valid_Address_Type;
            N_Old : Nodes.RW_Node_Type;
            I     : Nodes.Valid_Index_Type;
            N     : Nodes.RW_Node_Type;
         begin
            Pop_Inner(C_A, N_A, N_Old);
            I := Nodes.Child_Position(N_Old, C_A);
            N := Nodes.Substitution(N_Old, I, C_Key, C_A);
            Write_And_Ascend(N_A, N_Old, N);
         end;
      end if;
   end Update_High_Key;


   -- Handles the split of a node into nodes L and R, where L is written back to
   -- the position of the original node, L_A, and R is written to a new address,
   -- R_A.
   -- If there is no parent into which the subtree induced by R can be inserted,
   -- i.e. the stack is empty, then we've arrived at the root and create a new
   -- one.
   -- Otherwise, in the parent N of L, the key of the pointer to L is set to the
   -- possibly changed high-key of L and the pointer to R and its high-key are
   -- inserted.
   procedure Insert_Key_And_Update_High_Key
     (L_Key : in Key_Type;
      L_A   : in Nodes.Valid_Address_Type;
      R_Key : in Key_Type;
      R_A   : in Nodes.Valid_Address_Type)
   is
      use type Nodes.Valid_Address_Type;
   begin
      if Stack.Is_Empty then
         -- Create a new root that points to L and R.
         pragma Assert (L_A = Root_Address);
         declare
            use type Nodes.Level_Type;
            N_A     : constant Nodes.Valid_Address_Type := L_A;
            N       : Nodes.RW_Node_Type;
            L       : Nodes.RW_Node_Type;
            L_A_New : Nodes.Valid_Address_Type;
         begin
            Read_Node(Tree, L_A, L);
            pragma Assert (Nodes.Valid_Link(L) = R_A);
            Write_New_Node(Tree, L_A_New, L);
            N := Nodes.Root_Node(Nodes.Level(L) + 1);
            N := Nodes.Insertion(N, 1, L_Key, L_A_New);
            N := Nodes.Insertion(N, 2, R_Key, R_A);
            Nodes.Set_Link(N, Invalid_Address);
            Write_Node(Tree, N_A, N);
         exception
            when others =>
               Unlock(Tree, L_A);
               Unlock(Tree, R_A);
               raise;
         end;
         Unlock(Tree, L_A);
         Unlock(Tree, R_A);
         State := Success;
      else
         -- Update high key of L and insert high key of R.
         declare
            use type Nodes.Index_Type;
            N_A   : Nodes.Valid_Address_Type;
            N_Old : Nodes.RW_Node_Type;
            I     : Nodes.Index_Type;
            N     : Nodes.RW_Node_Type;
         begin
            declare
            begin
               Pop_Inner(L_A, N_A, N_Old);
            exception
               when others =>
                  Unlock(Tree, R_A);
                  raise;
            end;
            Unlock(Tree, R_A);
            I := Nodes.Child_Position(N_Old, L_A);
            N := Nodes.Substitution(N_Old, I, L_Key, L_A);
            I := I + 1;
            N := Nodes.Insertion(N, I, R_Key, R_A);
            Write_And_Ascend(N_A, N_Old, N);
         end;
      end if;
   end Insert_Key_And_Update_High_Key;


   -- Writes back the node(s) visited of the current level.
   -- The address N_A must be locked when this procedure is called.
   -- N_A is unlocked by this procedure.
   procedure Write_And_Ascend
     (N_A   : in Nodes.Valid_Address_Type;
      N_Old : in Nodes.RW_Node_Type;
      N     : in Nodes.RW_Node_Type)
   is
      use type Nodes.Degree_Type;
   begin
      if Nodes.Is_Safe(N, Is_Root => Stack.Is_Empty) then
         declare
         begin
            Write_Node(Tree, N_A, N);
         exception
            when others =>
               Unlock(Tree, N_A);
               raise;
         end;
         if not Nodes.Has_High_Key(N_Old) or else
            Nodes.High_Key(N_Old) /= Nodes.High_Key(N) then
            Update_High_Key(Nodes.High_Key(N), N_A);
         else
            Unlock(Tree, N_A);
            State := Success;
         end if;
      else
         declare
            I   : constant Nodes.Valid_Index_Type := Nodes.Split_Position(N);
            L   : Nodes.RW_Node_Type := Nodes.Copy(N, 1, I - 1);
            R   : Nodes.RW_Node_Type := Nodes.Copy(N, I, Nodes.Degree(N));
            L_A : Nodes.Valid_Address_Type renames N_A;
            R_A : Nodes.Valid_Address_Type;
         begin
            Nodes.Set_Link(R, Nodes.Link(N));
            Write_New_Node(Tree, R_A, R);
            Lock(Tree, R_A);
            Nodes.Set_Link(L, R_A);
            Write_Node(Tree, L_A, L);
            Insert_Key_And_Update_High_Key(Nodes.High_Key(L), L_A,
                                           Nodes.High_Key(R), R_A);
         end;
      end if;
   end Write_And_Ascend;

   use type Nodes.Valid_Index_Type;
   use type Blocks.Size_Type;
   N_A   : Nodes.Valid_Address_Type;
   N_Old : Nodes.RW_Node_Type;
   N     : Nodes.RW_Node_Type;
begin
   if Key_Size_Bound(Key) > Max_Key_Size(Value_Size_Bound(Value)) then
      State := Failure;
      return;
   end if;

   Stack.Initialize;

   declare
      use type Nodes.Level_Type;
      use type Nodes.Valid_Address_Type;

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
      Build_Stack(Nodes.Leaf_Level);
      Stack.Pop(N_A);
      Move_Right(Nodes.Leaf_Level, Exit_Cond'Access, N_A, N_Old);
   end;

   declare
      I : Nodes.Index_Type;
   begin
      I := Nodes.Key_Position(N_Old, Key);
      if not Nodes.Is_Valid(I) then
         I := Nodes.Degree(N_Old) + 1;
      elsif not Allow_Duplicates and then Nodes.Key(N_Old, I) = Key then
         State := Failure;
         return;
      end if;
      N := Nodes.Insertion(N_Old, I, Key, Value);
   exception
      when others =>
         Unlock(Tree, N_A);
         raise;
   end;
   Write_And_Ascend(N_A, N_Old, N);

   Stack.Finalize;

exception
   when others =>
      Stack.Finalize;
      pragma Warnings (Off);
      State := Error;
      pragma Warnings (On);
      raise;
end Insert;

