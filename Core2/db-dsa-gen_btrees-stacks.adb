-- Abstract:
--
-- Facilities to build a stack of nodes that represents a path from the root to
-- a node of some level, for example a leaf, that is the first candidate to
-- contain a certain key.
-- The stack is tightly coupled with the key; the key is set in the
-- initialization of the stack and cannot be changed.
--
-- This package is used by both, the insertion and deletion (check the
-- deletion's documentation to see why it actually needs the stack).
--
-- The descent (Build_Stack):
-- The stack is built by performing a descent using the Scan_Node procedure,
-- hence at each level walk to the right until the node induces the subtree
-- that's ought to contain Key.
-- During the descent, no locks are held (except for the root node for a short
-- time in case the first, non-locked attempt to read it returned a node that
-- was not a root but the left node of a just split root).
--
-- The ascent (Write_And_Ascend):
-- When Write_And_Ascend is called, the given node is checked whether it is safe
-- or not.
-- If it is not safe, it is split into two nodes and in the parent node the
-- pointer to the original node is replaced with the high key of the left node
-- and the high key of the right node is inserted.
-- If the node is safe but the high key has changed, the key of the pointer to
-- the node in the parent is updated to the new high key.
-- In both cases, the backtracking continues recursively.
-- Otherwise, everything is fine and the backtracking stops.
--
-- Design Notes:
--
-- General:
-- We have the handling procedures Write_And_Ascend, Update_High_Key, and
-- Insert_Key_And_Update_High_Key.
-- Write_And_Ascend terminates the handling of the current level. If necessary,
-- it ascends by calling Update_High_Key or Insert_Key_And_Update_High_Key on
-- the parent level.
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
--   Not earlier than in the next step in the ascent the root might be
--   reconsidered. If the level has changed since the stack was rebuilt, it is
--   rebuilt again and we are again below the root. This repeats until some time
--   the root wasn't split between the stack was built and the ascent has
--   reached the root.
-- * Pop_Inner's search for a node that has a pointer works because it is
--   guaranteed that the pointer to C_A has already been inserted. This is
--   because we use thread-safe Move_Right in the search in Pop_Inner.
--
-- Locks Count:
-- * Build_Stack usually locks nothing. Only if the read root is not a root
--   (because the root was just split), the Root_Address is locked and re-read.
--   This could also be realized with a loop/goto, but probably a lock is more
--   efficient. The lock is released immediately after re-reading the root.
-- * Write_And_Ascend locks many nodes to ensure thread-safety.
--   If a node is split, two locks are held: the original address of the node
--   (where the left one of the two new nodes is written) and the position where
--   the new right node is written.
--   When we step up to the above level, at most two more locks are held.
--   Hence, the maximum count of simultaneous locks during the ascend is four.
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
-- Copyright 2008--2011 Christoph Schwering

separate (DB.DSA.Gen_BTrees)
package body Stacks is

   type Item_Type is
      record
         Address : Nodes.Valid_Address_Type;
         Level   : Nodes.Level_Type;
      end record;

   package Stacks is new Utils.Gen_Stacks
     (Item_Type    => Item_Type,
      Initial_Size => 7,
      Storage_Pool => DB.Utils.Global_Pool.Global_Storage_Pool);

   type Stack_Type is
      record
         S   : Stacks.Stack_Type;
         Key : Keys.Key_Type;
      end record;


   procedure Initialize
     (Stack : out Stack_Type;
      Key   : in  Keys.Key_Type);

   procedure Finalize
     (Stack : in out Stack_Type);

   function Is_Empty
     (Stack : Stack_Type)
      return Boolean;

   procedure Pop
     (Stack : in out Stack_Type;
      N_A   :    out Nodes.Valid_Address_Type);

   procedure Pop
     (Stack : in out Stack_Type;
      N_A   :    out Nodes.Valid_Address_Type;
      Level :    out Nodes.Level_Type);

   procedure Build_Stack
     (Tree  : in out Tree_Type;
      Stack : in out Stack_Type;
      Level : in     Nodes.Level_Type);
   -- Builds the stack from the root to the outermost-left leaf that might
   -- contain the Key associated with the stack.

   generic
      with function Exit_Cond
        (N : Nodes.Node_Type)
         return Boolean;
   procedure Gen_Rebuilding_Move_Right
     (Tree  : in out Tree_Type;
      Stack : in out Stack_Type;
      Level : in     Nodes.Level_Type;
      N_A   : in out Nodes.Valid_Address_Type;
      N     :    out Nodes.Node_Type);
   -- Moves to the right using Gen_BTrees.Move_Right but handles potential
   -- splits of the tree correctly. If the nodes read starting from N_A are
   -- not from the expected Level, the Stack is re-built.

   procedure Write_And_Ascend
     (Tree  : in out Tree_Type;
      Stack : in out Stack_Type;
      N_A   : in     Nodes.Valid_Address_Type;
      N_Old : in     Nodes.RW_Node_Type;
      N     : in     Nodes.RW_Node_Type);
   -- Writes back the node(s) visited of the current level.
   -- The address N_A must be locked when this procedure is called.
   -- N_A is unlocked by this procedure.


   procedure Initialize
     (Stack : out Stack_Type;
      Key   : in  Keys.Key_Type) is
   begin
      Stack := (S => Stacks.New_Stack, Key => Key);
   end Initialize;


   procedure Finalize
     (Stack : in out Stack_Type) is
   begin
      Stacks.Finalize (Stack.S);
   end Finalize;


   procedure Clear
     (Stack : in out Stack_Type) is
   begin
      Stacks.Clear (Stack.S);
   end Clear;


   function Is_Empty
     (Stack : Stack_Type)
      return Boolean is
   begin
      return Stacks.Is_Empty (Stack.S);
   end Is_Empty;


   procedure Push
     (Stack : in out Stack_Type;
      N_A   : in Nodes.Valid_Address_Type;
      Level : in Nodes.Level_Type) is
   begin
      Stacks.Push (Stack.S, Item_Type'(N_A, Level));
   end Push;


   procedure Pop
     (Stack : in out Stack_Type;
      N_A   :    out Nodes.Valid_Address_Type)
   is
      Level : Nodes.Level_Type;
   begin
      Pop (Stack, N_A, Level);
   end Pop;


   procedure Pop
     (Stack : in out Stack_Type;
      N_A   :    out Nodes.Valid_Address_Type;
      Level :    out Nodes.Level_Type)
   is
      Item : Item_Type;
   begin
      Stacks.Pop (Stack.S, Item);
      N_A := Item.Address;
      Level := Item.Level;
   end Pop;


   procedure Build_Stack
     (Tree  : in out Tree_Type;
      Stack : in out Stack_Type;
      Level : in     Nodes.Level_Type)
   is
      use type Nodes.Level_Type;
      N_A : Nodes.Valid_Address_Type;
      N   : Nodes.RO_Node_Type;
   begin
      Clear (Stack);

      N_A := Root_Address;
      Read_Node (Tree, N_A, N);
      if Nodes.Is_Valid (Nodes.Link (N)) then
         -- Lock root to enforce that it is the first node in the stack.
         Lock (Tree, N_A);
         begin
            Read_Node (Tree, N_A, N);
            pragma Assert (not Nodes.Is_Valid (Nodes.Link (N)));
         exception
            when others =>
               Unlock (Tree, N_A);
               raise;
         end;
         Unlock (Tree, N_A);
      end if;

      loop
         if Nodes.Level (N) = Level then
            Push (Stack, N_A, Nodes.Level (N));
            exit;
         end if;
         pragma Assert (Nodes.Level (N) > Level);
         declare
            use type Nodes.Address_Type;
            NN_A : constant Nodes.Valid_Address_Type :=
              Scan_Node (N, Stack.Key);
         begin
            if Nodes.To_Address (NN_A) /= Nodes.Link (N) then
               Push (Stack, N_A, Nodes.Level (N));
            end if;
            N_A := NN_A;
         end;
         Read_Node (Tree, N_A, N);
      end loop;
   end Build_Stack;


   procedure Gen_Rebuilding_Move_Right
     (Tree  : in out          Tree_Type;
      Stack : in out          Stack_Type;
      Level : in              Nodes.Level_Type;
      N_A   : in out          Nodes.Valid_Address_Type;
      N     :    out          Nodes.Node_Type)
   is
      use type Nodes.Level_Type;
      use type Nodes.Valid_Address_Type;

      function Precise_Exit_Cond (N : Nodes.Node_Type) return Boolean;
      pragma Inline (Precise_Exit_Cond);

      function Precise_Exit_Cond (N : Nodes.Node_Type) return Boolean is
      begin
         if Nodes.Level (N) /= Level then
            pragma Assert (Is_Empty (Stack));
            return True;
         end if;
         return Exit_Cond (N);
      end Precise_Exit_Cond;

      procedure Move_Right is new Gen_Move_Right (Precise_Exit_Cond);
   begin
      Move_Right (Tree, N_A, N);
      if Nodes.Level (N) /= Level then
         pragma Assert (N_A = Root_Address);
         Unlock (Tree, N_A);
         Build_Stack (Tree, Stack, Level);
         Pop (Stack, N_A);
         pragma Assert (N_A /= Root_Address);
         Move_Right (Tree, N_A, N);
         pragma Assert (Nodes.Level (N) = Level);
      end if;
   end Gen_Rebuilding_Move_Right;


   procedure Write_And_Ascend
     (Tree  : in out Tree_Type;
      Stack : in out Stack_Type;
      N_A   : in     Nodes.Valid_Address_Type;
      N_Old : in     Nodes.RW_Node_Type;
      N     : in     Nodes.RW_Node_Type)
   is
      procedure Pop_Inner
        (C_A : in  Nodes.Valid_Address_Type;
         N_A : out Nodes.Valid_Address_Type;
         N   : out Nodes.RW_Node_Type);

      procedure Update_High_Key
        (C_Key : in Keys.Key_Type;
         C_A   : in Nodes.Valid_Address_Type);
      -- Handles the case that the high-key of a node C changed. Then C_A is the
      -- address of C and C_Key is the high-key of C.

      procedure Insert_Key_And_Update_High_Key
        (L_Key : in Keys.Key_Type;
         L_A   : in Nodes.Valid_Address_Type;
         R_Key : in Keys.Key_Type;
         R_A   : in Nodes.Valid_Address_Type);
      -- Handles the split of a node into nodes L and R, where L is written back
      -- to the position of the original node, L_A, and R is written to a new
      -- address, R_A.
      -- If there is no parent into which the subtree induced by R can be
      -- inserted, i.e. the stack is empty, then we've arrived at the root and
      -- create a new one.
      -- Otherwise, in the parent N of L, the key of the pointer to L is set to
      -- the possibly changed high-key of L and the pointer to R and its
      -- high-key are inserted.

      procedure Write_And_Ascend
        (N_A   : in Nodes.Valid_Address_Type;
         N_Old : in Nodes.RW_Node_Type;
         N     : in Nodes.RW_Node_Type);


      procedure Pop_Inner
        (C_A : in  Nodes.Valid_Address_Type;
         N_A : out Nodes.Valid_Address_Type;
         N   : out Nodes.RW_Node_Type)
      is
         function Exit_Cond (N : Nodes.Node_Type) return Boolean is
         begin
            return Nodes.Is_Valid (Nodes.Child_Position (N, C_A));
         end Exit_Cond;

         procedure Move_Right is new Gen_Rebuilding_Move_Right (Exit_Cond);

         Level : Nodes.Level_Type;
      begin
         begin
            Pop (Stack, N_A, Level);
            Move_Right (Tree, Stack, Level, N_A, N);
         exception
            when others =>
               Unlock (Tree, C_A);
               raise;
         end;
         Unlock (Tree, C_A);
      end Pop_Inner;


      procedure Update_High_Key
        (C_Key : in Keys.Key_Type;
         C_A   : in Nodes.Valid_Address_Type) is
      begin
         if Is_Empty (Stack) then
            Unlock (Tree, C_A);
         else
            declare
               N_A   : Nodes.Valid_Address_Type;
               N_Old : Nodes.RW_Node_Type;
               I     : Nodes.Index_Type;
               N     : Nodes.RW_Node_Type;
            begin
               Pop_Inner (C_A, N_A, N_Old);
               I := Nodes.Child_Position (N_Old, C_A);
               N := Nodes.Substitution (N_Old, I, C_Key, C_A);
               Write_And_Ascend (N_A, N_Old, N);
            end;
         end if;
      end Update_High_Key;


      procedure Insert_Key_And_Update_High_Key
        (L_Key : in Keys.Key_Type;
         L_A   : in Nodes.Valid_Address_Type;
         R_Key : in Keys.Key_Type;
         R_A   : in Nodes.Valid_Address_Type)
      is
         use type Nodes.Valid_Address_Type;
      begin
         if Is_Empty (Stack) then
            -- Create a new root that points to L and R.
            pragma Assert (L_A = Root_Address);
            declare
               use type Nodes.Level_Type;
               N_A     : constant Nodes.Valid_Address_Type := L_A;
               N       : Nodes.RW_Node_Type;
               L       : Nodes.RW_Node_Type;
               L_A_New : Nodes.Valid_Address_Type;
            begin
               Read_Node (Tree, L_A, L);
               pragma Assert (Nodes.Valid_Link (L) = R_A);
               Write_New_Node (Tree, L_A_New, L);
               N := Nodes.Root_Node (Nodes.Level (L) + 1);
               N := Nodes.Insertion (N, 1, L_Key, L_A_New);
               N := Nodes.Insertion (N, 2, R_Key, R_A);
               Nodes.Set_Link (N, Invalid_Address);
               Write_Node (Tree, N_A, N);
            exception
               when others =>
                  Unlock (Tree, L_A);
                  Unlock (Tree, R_A);
                  raise;
            end;
            Unlock (Tree, L_A);
            Unlock (Tree, R_A);
         else
            -- Update high key of L and insert high key of R.
            declare
               use type Nodes.Extended_Index_Type;
               N_A   : Nodes.Valid_Address_Type;
               N_Old : Nodes.RW_Node_Type;
               I     : Nodes.Extended_Index_Type;
               N     : Nodes.RW_Node_Type;
            begin
               begin
                  Pop_Inner (L_A, N_A, N_Old);
               exception
                  when others =>
                     Unlock (Tree, R_A);
                     raise;
               end;
               Unlock (Tree, R_A);
               I := Nodes.Child_Position (N_Old, L_A);
               N := Nodes.Substitution (N_Old, I, L_Key, L_A);
               I := I + 1;
               N := Nodes.Insertion (N, I, R_Key, R_A);
               Write_And_Ascend (N_A, N_Old, N);
            end;
         end if;
      end Insert_Key_And_Update_High_Key;


      procedure Write_And_Ascend
        (N_A   : in Nodes.Valid_Address_Type;
         N_Old : in Nodes.RW_Node_Type;
         N     : in Nodes.RW_Node_Type)
      is
         use type Nodes.Degree_Type;
      begin
         if Nodes.Is_Safe (N, Is_Root => Is_Empty (Stack)) then
            begin
               Write_Node (Tree, N_A, N);
            exception
               when others =>
                  Unlock (Tree, N_A);
                  raise;
            end;
            if not Nodes.Has_High_Key (N_Old) or else
               Nodes.High_Key (N_Old) /= Nodes.High_Key (N)
            then
               Update_High_Key (Nodes.High_Key (N), N_A);
            else
               Unlock (Tree, N_A);
            end if;
         else
            declare
               I : constant Nodes.Index_Type := Nodes.Split_Position (N);
               L : Nodes.RW_Node_Type := Nodes.Copy (N, 1, I - 1);
               R : Nodes.RW_Node_Type := Nodes.Copy (N, I, Nodes.Degree (N));
               L_A : Nodes.Valid_Address_Type renames N_A;
               R_A : Nodes.Valid_Address_Type;
            begin
               Nodes.Set_Link (R, Nodes.Link (N));
               Write_New_Node (Tree, R_A, R);
               Lock (Tree, R_A);
               begin
                  Nodes.Set_Link (L, R_A);
                  Write_Node (Tree, L_A, L);
               exception
                  when others =>
                     Unlock (Tree, R_A);
                     raise;
               end;
               Insert_Key_And_Update_High_Key (Nodes.High_Key (L), L_A,
                                               Nodes.High_Key (R), R_A);
            end;
         end if;
      end Write_And_Ascend;

   begin
      Write_And_Ascend (N_A, N_Old, N);
   end Write_And_Ascend;


   procedure Gen_Modify
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      State :    out State_Type)
   is
      pragma Precondition (Tree.Initialized);

      use type Nodes.Index_Type;
      use type Blocks.Size_Type;

      procedure Move_Right is new Gen_Rebuilding_Move_Right (Exit_Cond);

      Stack : Stack_Type;
      N_A   : Nodes.Valid_Address_Type;
      N_Old : Nodes.RW_Node_Type;
   begin
      Initialize (Stack, Key);

      Build_Stack (Tree, Stack, Nodes.Leaf_Level);
      Pop (Stack, N_A);
      Move_Right (Tree, Stack, Nodes.Leaf_Level, N_A, N_Old);

      declare
         N : Nodes.RW_Node_Type;
      begin
         Modify_Node (N_Old, N, State);
         case State is
            when Success =>
               Write_And_Ascend (Tree, Stack, N_A, N_Old, N);
            when Failure =>
               Unlock (Tree, N_A);
         end case;
      exception
         when others =>
            Unlock (Tree, N_A);
            raise;
      end;

      Finalize (Stack);

   exception
      when others =>
         Finalize (Stack);
         raise;
   end Gen_Modify;

end Stacks;

