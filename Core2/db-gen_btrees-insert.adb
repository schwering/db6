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
--       into the parent.
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

with Interfaces.C;

with DB.Utils.Gen_Stacks;
with DB.Utils.Global_Pool;
with DB.Utils.Print; use DB.Utils;

separate (DB.Gen_BTrees)
procedure Insert
  (Tree  : in out Tree_Type;
   Key   : in     Key_Type;
   Value : in     Value_Type;
   State :    out State_Type)
is
   pragma Assert (Tree.Initialized);

   procedure Sleep
   is
      use Interfaces.C;
      function C_Rand return Int;
      pragma Import (C, C_Rand, "rand");
      function C_Sleep (Secs : Int) return Int;
      pragma Import (C, C_Sleep, "usleep");
      R : constant Int := C_Rand mod 77;
      D : constant Int := R * 100000;
   begin
      if R = 4 then
         declare
            I : constant Int := C_Sleep(D);
            pragma Unreferenced (I);
         begin
            Print("Sleeped for "& D'Img &" usecs");
         end;
      end if;
   end Sleep;

   type Stack_Item_Type is
      record
         Address : Nodes.Valid_Address_Type;
         Level   : Nodes.Level_Type;
      end record;

   package Stacks is new Utils.Gen_Stacks
     (Item_Type    => Stack_Item_Type,
      Initial_Size => 7,
      Storage_Pool => Utils.Global_Pool.Global'Storage_Pool);

   Stack : Stacks.Stack_Type;

   procedure Stack_Pop
     (N_A   : out Nodes.Valid_Address_Type;
      Level : out Nodes.Level_Type)
   is
      Item : Stack_Item_Type;
   begin
      Stacks.Pop(Stack, Item);
      N_A := Item.Address;
      Level := Item.Level;
   end Stack_Pop;

   procedure Stack_Push
     (N_A   : in Nodes.Valid_Address_Type;
      Level : in Nodes.Level_Type) is
   begin
      Stacks.Push(Stack, Stack_Item_Type'(N_A, Level));
   end Stack_Push;

   procedure Build_Stack
   is
      N_A : Nodes.Valid_Address_Type := Root_Address;
      N   : Nodes.RO_Node_Type;
   begin
      Stacks.Clear(Stack);
      Read_Node(Tree, N_A, N);
      loop
         if Nodes.Is_Inner(N) then
            declare
               use type Nodes.Address_Type;
               NN_A : constant Nodes.Valid_Address_Type := Scan_Node(N, Key);
            begin
               if Nodes.To_Address(NN_A) /= Nodes.Link(N) then
                  Stack_Push(N_A, Nodes.Level(N));
               end if;
               N_A := NN_A;
            end;
            Read_Node(Tree, N_A, N);
         else
            Stack_Push(N_A, Nodes.Level(N));
            exit;
         end if;
      end loop;
   end Build_Stack;

   procedure Rebuild_Stack
     (Level : in  Nodes.Level_Type;
      C_A   : in  Nodes.Valid_Address_Type)
   is
      use type Nodes.Level_Type;
      N_A : Nodes.Valid_Address_Type := Root_Address;
      N   : Nodes.RO_Node_Type;
   begin
      Stacks.Clear(Stack);
      Read_Node(Tree, N_A, N);
      loop
         pragma Assert (Nodes.Is_Inner(N));
         if Nodes.Level(N) /= Level then
            declare
               use type Nodes.Address_Type;
               NN_A : constant Nodes.Valid_Address_Type := Scan_Node(N, Key);
            begin
               if Nodes.To_Address(NN_A) /= Nodes.Link(N) then
                  Stack_Push(N_A, Nodes.Level(N));
               end if;
               N_A := NN_A;
            end;
            Read_Node(Tree, N_A, N);
         else
            declare
               I : constant Nodes.Index_Type := Nodes.Child_Position(N, C_A);
            begin
               if not Nodes.Is_Valid(I) then
                  N_A := Nodes.Valid_Link(N);
                  Read_Node(Tree, N_A, N);
               else
                  Stack_Push(N_A, Nodes.Level(N));
                  exit;
               end if;
            end;
         end if;
      end loop;
   end Rebuild_Stack;

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
     (N_A :    out Nodes.Valid_Address_Type;
      N   :    out Nodes.RW_Node_Type)
   is
      use type Nodes.Valid_Address_Type;

      Rebuild : Boolean;
      Level   : Nodes.Level_Type;

      function Exit_Condition
        (N : Nodes.Node_Type)
         return Boolean
      is
         High_Key     : Key_Type;
         Has_High_Key : Boolean;
      begin
         if Stacks.Is_Empty(Stack) and not Nodes.Is_Leaf(N) then
            Rebuild := True;
            return True;
         end if;
         if not Nodes.Is_Valid(Nodes.Link(N)) then
            return True;
         end if;
         Nodes.Get_High_Key(N, High_Key, Has_High_Key);
         return Has_High_Key and then Key <= High_Key;
      end Exit_Condition;
   begin
      <<Retry>>
      Stack_Pop(N_A, Level);
      Rebuild := False;
      Move_Right(Tree, Exit_Condition'Access, N_A, N);
      if Rebuild then
         Unlock(Tree, N_A);
         Build_Stack;
         goto Retry;
      end if;

      declare
      begin
         if not Nodes.Is_Leaf(N) then
            raise Tree_Error;
         end if;
      exception
         when others =>
            Unlock(Tree, N_A);
            raise;
      end;
   end Pop_Leaf;

   procedure Pop_Inner
     (C_A : in     Nodes.Valid_Address_Type;
      N_A :    out Nodes.Valid_Address_Type;
      N   :    out Nodes.RW_Node_Type)
   is
      use type Nodes.Valid_Address_Type;
      use type Nodes.Level_Type;

      Rebuild : Boolean;
      Level   : Nodes.Level_Type;

      function Exit_Condition
        (N : Nodes.Node_Type)
         return Boolean is
      begin
         if Stacks.Is_Empty(Stack) and Nodes.Level(N) /= Level then
            Rebuild := True;
            return True;
         end if;
         return Nodes.Is_Valid(Nodes.Child_Position(N, C_A));
      end Exit_Condition;
   begin
      declare
      begin
         <<Retry>>
         Stack_Pop(N_A, Level);
         Rebuild := False;
         Move_Right(Tree, Exit_Condition'Access, N_A, N);
         if Rebuild then
            Unlock(Tree, N_A);
            Rebuild_Stack(Level, C_A);
            goto Retry;
         end if;
      exception
         when others =>
            Unlock(Tree, C_A);
            raise;
      end;
      Unlock(Tree, C_A);

      declare
      begin
         if not Nodes.Is_Inner(N) then
            raise Tree_Error;
         end if;
      exception
         when others =>
            Unlock(Tree, N_A);
            raise;
      end;
   end Pop_Inner;

   procedure Write_And_Ascend
     (N_A   : in     Nodes.Valid_Address_Type;
      N_Old : in     Nodes.RW_Node_Type;
      N     : in     Nodes.RW_Node_Type);

   -- Handles the case that the high-key of a node C changed. Then C_A is the
   -- address of C and C_Key is the high-key of C.
   procedure Update_High_Key
     (C_Key : in     Key_Type;
      C_A   : in     Nodes.Valid_Address_Type) is
   begin
      if Stacks.Is_Empty(Stack) then
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
            declare
            begin
               I := Nodes.Child_Position(N_Old, C_A);
               N := Nodes.Substitution(N_Old, I, C_Key, C_A);
            exception
               when others =>
                  Unlock(Tree, N_A);
                  raise;
            end;
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
   -- The addresses L_A and R_A must be locked when this procedure is called.
   -- L_A and R_A are unlocked by this procedure.
   procedure Insert_Key_And_Update_High_Key
     (L_Key : in     Key_Type;
      L_A   : in     Nodes.Valid_Address_Type;
      R_Key : in     Key_Type;
      R_A   : in     Nodes.Valid_Address_Type)
   is
      use type Nodes.Valid_Address_Type;
   begin
      if Stacks.Is_Empty(Stack) then
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
            N := Nodes.Root_Node(Is_Leaf => False, Level => Nodes.Level(L) + 1);
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
            declare
               I : Nodes.Index_Type;
               N : Nodes.RW_Node_Type;
            begin
               I := Nodes.Child_Position(N_Old, L_A);
               N := Nodes.Substitution(N_Old, I, L_Key, L_A);
               I := I + 1;
               N := Nodes.Insertion(N, I, R_Key, R_A);
               Write_And_Ascend(N_A, N_Old, N);
            end;
         end;
      end if;
   end Insert_Key_And_Update_High_Key;

   -- Writes back the node(s) visited of the current level.
   -- The address N_A must be locked when this procedure is called.
   -- N_A is unlocked by this procedure.
   procedure Write_And_Ascend
     (N_A   : in     Nodes.Valid_Address_Type;
      N_Old : in     Nodes.RW_Node_Type;
      N     : in     Nodes.RW_Node_Type)
   is
      use type Nodes.Degree_Type;
   begin
      if Nodes.Is_Safe(N, Is_Root => Stacks.Is_Empty(Stack)) then
         declare
         begin
            Write_Node(Tree, N_A, N);
         exception
            when others =>
               Unlock(Tree, N_A);
               raise;
         end;
         if Nodes.Degree(N_Old) = 0 or else High_Key(N_Old) /= High_Key(N) then
            Update_High_Key(High_Key(N), N_A);
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
            Insert_Key_And_Update_High_Key(High_Key(L), L_A,
                                           High_Key(R), R_A);
         end;
      end if;
   end Write_And_Ascend;

   use type Nodes.Valid_Index_Type;
   use type Blocks.Size_Type;
   N_A   : Nodes.Valid_Address_Type;
   N_Old : Nodes.RW_Node_Type;
   I     : Nodes.Index_Type;
   N     : Nodes.RW_Node_Type;
begin
   if Key_Size_Bound(Key) > Max_Key_Size(Value_Size_Bound(Value)) then
      State := Failure;
      return;
   end if;

   Stack := Stacks.New_Stack;

   Build_Stack;

   Pop_Leaf(N_A, N_Old);
   declare
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

   Stacks.Finalize(Stack);

exception
   when others =>
      Stacks.Finalize(Stack);
      pragma Warnings (Off);
      State := Error;
      pragma Warnings (On);
      raise;
end Insert;

