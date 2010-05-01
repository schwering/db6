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
--     Now start the ascent:
-- (3.a) If the current node is not safe, split it. Update the left node's
--       high key in parent and insert the new node's high key plus address
--       into the parent. XXX This is wrong in the implementation.
--       The parent is found via Move_Right which holds at most two locks
--       at once. Additionally the current node is locked, hence at most three
--       locks are held. At the end, the parent node is locked (and becomes
--       the current node and the ascent is continued recursively).
-- (3.b) If the inserted key is the high key of the new node, update the high
--       key in the parent. The parent is found the same way as in case (3.a).
-- (3.c) Everything is fine.
-- Hence the maximum count of simultaneously held locks is three.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils.Gen_Stacks;
with DB.Utils.Global_Pool;

separate (DB.Gen_BTrees)
procedure Insert
  (Tree     : in out Tree_Type;
   Key      : in     Key_Type;
   Value    : in     Value_Type;
   State    :    out State_Type)
is
   pragma Assert (Tree.Initialized);

   package Stacks is new Utils.Gen_Stacks
     (Item_Type    => Nodes.Valid_Address_Type,
      Initial_Size => 7,
      Storage_Pool => Utils.Global_Pool.Global'Storage_Pool);

   procedure Initialize_Stack
     (Stack : out Stacks.Stack_Type)
   is
      use type Nodes.Address_Type;
      N_A : Nodes.Valid_Address_Type := Root_Address;
      N   : Nodes.RO_Node_Type;
   begin
      Stack := Stacks.New_Stack;
      loop
         Read_Node(Tree, N_A, N);
         if Nodes.To_Address(N_A) /= Nodes.Link(N) then
            Stacks.Push(Stack, N_A);
         end if;
         exit when Nodes.Is_Leaf(N);
         N_A := Scan_Node(N, Key);
      end loop;
   end Initialize_Stack;

   function High_Key
     (N : Nodes.Node_Type)
      return Key_Type
   is
      use type Nodes.Degree_Type;
      pragma Assert (Nodes.Degree(N) > 0);
   begin
      return Nodes.Key(N, Nodes.Degree(N));
   end High_Key;

   procedure Pop_Leaf
     (Stack : in out Stacks.Stack_Type;
      N_A   :    out Nodes.Valid_Address_Type;
      N     :    out Nodes.RW_Node_Type;
      State : in out State_Type) is
   begin
      Stacks.Pop(Stack, N_A);
      Move_Right_To_Key(Tree, Key, N_A, N);
      declare
      begin
         if not Nodes.Is_Leaf(N) then
            pragma Warnings (Off);
            State := Error;
            pragma Warnings (On);
            raise Tree_Error;
         end if;
      exception
         when others =>
            Unlock(Tree, N_A);
            raise;
      end;
   end Pop_Leaf;

   procedure Pop_Inner
     (Stack : in out Stacks.Stack_Type;
      C_A   : in     Nodes.Valid_Address_Type;
      N_A   :    out Nodes.Valid_Address_Type;
      N     :    out Nodes.RW_Node_Type;
      State : in out State_Type) is
   begin
      Stacks.Pop(Stack, N_A);
      Move_Right_To_Address(Tree, C_A, N_A, N);
      Unlock(Tree, C_A);
      declare
      begin
         if not Nodes.Is_Inner(N) then
            pragma Warnings (Off);
            State := Error;
            pragma Warnings (On);
            raise Tree_Error;
         end if;
      exception
         when others =>
            Unlock(Tree, N_A);
            raise;
      end;
   exception
      when others =>
         Unlock(Tree, C_A);
         raise;
   end Pop_Inner;

   procedure Write_And_Ascend
     (Stack : in out Stacks.Stack_Type;
      N_A   : in     Nodes.Valid_Address_Type;
      N_Old : in     Nodes.RW_Node_Type;
      N     : in     Nodes.RW_Node_Type;
      State : in out State_Type);

   -- Handles the case that the high-key of a node C changed. Then C_A is the
   -- address of C and C_Key is the high-key of C.
   procedure Update_High_Key
     (Stack : in out Stacks.Stack_Type;
      C_Key : in     Key_Type;
      C_A   : in     Nodes.Valid_Address_Type;
      State : in out State_Type) is
   begin
      if Stacks.Is_Empty(Stack) then
         Unlock(Tree, C_A);
         State := Success;
      else
         declare
            N_A   : Nodes.Valid_Address_Type;
            N_Old : Nodes.RW_Node_Type;
         begin
            Pop_Inner(Stack, C_A, N_A, N_Old, State);
            declare
               I : Nodes.Valid_Index_Type;
               N : Nodes.RW_Node_Type;
            begin
               I := Nodes.Child_Position(N_Old, C_A);
               N := Nodes.Substitution(N_Old, I, C_Key, C_A);
               Write_And_Ascend(Stack, N_A, N_Old, N, State);
            exception
               when others =>
                  Unlock(Tree, N_A);
                  raise;
            end;
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
     (Stack : in out Stacks.Stack_Type;
      L_Key : in     Key_Type;
      L_A   : in     Nodes.Valid_Address_Type;
      R_Key : in     Key_Type;
      R_A   : in     Nodes.Valid_Address_Type;
      State : in out State_Type)
   is
      use type Nodes.Valid_Address_Type;
   begin
      if Stacks.Is_Empty(Stack) then
         -- Create a new root that points to L and R.
         pragma Assert (L_A = Root_Address);
         declare
            N_A     : constant Nodes.Valid_Address_Type := L_A;
            N       : Nodes.RW_Node_Type := Nodes.Root_Node(Is_Leaf => False);
            L       : Nodes.RW_Node_Type;
            L_A_New : Nodes.Valid_Address_Type;
         begin
            Read_Node(Tree, L_A, L);
            pragma Assert (Nodes.Valid_Link(L) = R_A);
            Write_New_Node(Tree, L_A_New, L);
            N := Nodes.Insertion(N, 1, L_Key, L_A_New);
            N := Nodes.Insertion(N, 2, R_Key, R_A);
            Nodes.Set_Link(N, Invalid_Address);
            Write_Node(Tree, N_A, N);
            Unlock(Tree, L_A);
            State := Success;
         end;
      else
         -- Update high key of L and insert high key of R.
         declare
            use type Nodes.Index_Type;
            N_A   : Nodes.Valid_Address_Type;
            N_Old : Nodes.RW_Node_Type;
         begin
            Pop_Inner(Stack, L_A, N_A, N_Old, State);
            declare
               I : Nodes.Index_Type;
               N : Nodes.RW_Node_Type;
            begin
               I := Nodes.Child_Position(N_Old, L_A);
               N := Nodes.Substitution(N_Old, I, L_Key, L_A);
               I := I + 1;
               N := Nodes.Insertion(N, I, R_Key, R_A);
               Write_And_Ascend(Stack, N_A, N_Old, N, State);
            exception
               when others =>
                  Unlock(Tree, N_A);
                  raise;
            end;
         end;
      end if;
   end Insert_Key_And_Update_High_Key;

   procedure Write_And_Ascend
     (Stack : in out Stacks.Stack_Type;
      N_A   : in     Nodes.Valid_Address_Type;
      N_Old : in     Nodes.RW_Node_Type;
      N     : in     Nodes.RW_Node_Type;
      State : in out State_Type)
   is
      use type Nodes.Degree_Type;
   begin
      if Nodes.Is_Safe(N, Is_Root => Stacks.Is_Empty(Stack)) then
         Write_Node(Tree, N_A, N);
         if Nodes.Degree(N_Old) = 0 or else High_Key(N_Old) /= High_Key(N) then
            Update_High_Key(Stack, High_Key(N), N_A, State);
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
            Nodes.Set_Link(L, R_A);
            Write_Node(Tree, L_A, L);
            Insert_Key_And_Update_High_Key(Stack,
                                           High_Key(L), L_A,
                                           High_Key(R), R_A,
                                           State);
         end;
      end if;
   end Write_And_Ascend;

   use type Nodes.Valid_Index_Type;
   use type Blocks.Size_Type;
   Stack : Stacks.Stack_Type;
   N_A   : Nodes.Valid_Address_Type;
   N_Old : Nodes.RW_Node_Type;
   I     : Nodes.Index_Type;
   N     : Nodes.RW_Node_Type;
begin
   State := Success;
   if Key_Size_Bound(Key) > Max_Key_Size(Value_Size_Bound(Value)) then
      State := Failure;
      return;
   end if;

   Initialize_Stack(Stack);

   Pop_Leaf(Stack, N_A, N_Old, State);
   if State /= Success then
      return;
   end if;
   I := Nodes.Key_Position(N_Old, Key);
   if not Nodes.Is_Valid(I) then
      I := Nodes.Degree(N_Old) + 1;
   elsif not Allow_Duplicates and then Nodes.Key(N_Old, I) = Key then
      State := Failure;
      return;
   end if;
   N := Nodes.Insertion(N_Old, I, Key, Value);
   Write_And_Ascend(Stack, N_A, N_Old, N, State);

exception
   when others =>
      Stacks.Finalize(Stack);
      raise;
end Insert;

