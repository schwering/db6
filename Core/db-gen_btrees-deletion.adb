separate (DB.Gen_BTrees)
package body Deletion is

   procedure Delete
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is
      pragma Assert (Tree.Initialized);
      Transaction : RW_Transaction_Type := New_RW_Transaction(Tree);
   begin
      Start_Transaction(Tree, Transaction);
      Delete(Tree, Transaction, Key, Value, Position, State);
      if State = Success then
         Commit_Transaction(Tree, Transaction);
      else
         Abort_Transaction(Tree, Transaction);
      end if;
   exception
      when others =>
         Abort_Transaction(Tree, Transaction);
         raise;
   end Delete;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      pragma Assert (Transaction.Started);
      N_A : Nodes.Valid_Address_Type;
      I   : Nodes.Index_Type;
   begin
      State := Success;

      -- Search leaf, fill Buffer. Correctly initialize N_A and I.
      N_A      := Transaction.Current_Root_Address;
      Position := 0;
      loop
         declare
            use type Nodes.Degree_Type;
            N : Nodes.Node_Type;
         begin
            Read_Node(Tree, Transaction, N_A, N);
            I := Nodes.Key_Position(N, Key);
            if not Nodes.Is_Valid(I) or else
               (Nodes.Is_Leaf(N) and then (Nodes.Key(N, I) /= Key)) then
               State := Failure;
               return;
            end if;
            if Nodes.Is_Leaf(N) then
               Position := Position + Nodes.Count_Sum(N, I);
               exit;
            end if;
            Position := Position + Nodes.Count_Sum(N, I-1);
            N_A      := Nodes.Child(N, I);
         end;
      end loop;

      -- Delete in memory and dispatch to add the Nodes.
      declare
         use type Nodes.Degree_Type;
         N_Old : Nodes.Node_Type;
         N_New : Nodes.Node_Type;
      begin
         Read_Node(Tree, Transaction, N_A, N_Old);
         N_New := Nodes.Deletion(N_Old, I);
         Value := Nodes.Value(N_Old, I);
         Handle_Underflow(Tree, Transaction, N_A, N_New, State);
      end;

   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Delete;


   procedure Delete
     (Tree     : in out Tree_Type;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type)
   is
      pragma Assert (Tree.Initialized);
      Transaction : RW_Transaction_Type := New_RW_Transaction(Tree);
   begin
      Start_Transaction(Tree, Transaction);
      Delete(Tree, Transaction, Position, Value, Key, State);
      if State = Success then
         Commit_Transaction(Tree, Transaction);
      else
         Abort_Transaction(Tree, Transaction);
      end if;
   exception
      when others =>
         Abort_Transaction(Tree, Transaction);
         raise;
   end Delete;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      pragma Assert (Transaction.Started);
      N_A : Nodes.Valid_Address_Type;
      I   : Nodes.Index_Type;
      Pos : Count_Type;
   begin
      State := Success;

      -- Search leaf, fill Buffer. Correctly initialize N_A and I.
      N_A := Transaction.Current_Root_Address;
      Pos := Position;
      loop
         declare
            use type Nodes.Degree_Type;
            N : Nodes.Node_Type;
         begin
            Read_Node(Tree, Transaction, N_A, N);
            I := Nodes.Count_Position(N, Pos);
            if not Nodes.Is_Valid(I) then
               State := Failure;
               return;
            end if;
            exit when Nodes.Is_Leaf(N);
            Pos := Pos - Nodes.Count_Sum(N, I-1);
            N_A := Nodes.Child(N, I);
         end;
      end loop;

      -- Delete in memory and dispatch to add the node.
      declare
         use type Nodes.Degree_Type;
         N_Old : Nodes.Node_Type;
         N_New : Nodes.Node_Type;
      begin
         Read_Node(Tree, Transaction, N_A, N_Old);
         N_New := Nodes.Deletion(N_Old, I);
         Key   := Nodes.Key(N_Old, I);
         Value := Nodes.Value(N_Old, I);
         Handle_Underflow(Tree, Transaction, N_A, N_New, State);
      end;

   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Delete;


   -- Dispatches to the appropriate next operation:
   -- 1. drop the empty empty, or
   -- 2. simply delete, or
   -- 3. shift the minimum from the right neighbor, or
   -- 4. shift the maximum from the left neighbor, or
   -- 5. merge with the right neighbor, or
   -- 6. merge with the left neighbor.
   procedure Handle_Underflow
     (Tree  : in out Tree_Type;
      T     : in out RW_Transaction_Type'Class;
      N_A   : in     Nodes.Valid_Address_Type;
      N     : in     Nodes.Node_Type;
      State : in out Result_Type)
   is

      procedure Handle_Empty_Root
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         N_A   : in     Nodes.Valid_Address_Type;
         N     : in     Nodes.Node_Type;
         State : in out Result_Type)
      is
         use type Nodes.Degree_Type;
      begin
         if Nodes.Is_Root(N) and Nodes.Degree(N) = 0 then
            Write_Node(Tree, T, N_A, N);
         elsif Nodes.Is_Root(N) and Nodes.Is_Inner(N) and
            Nodes.Degree(N) = 1 then
            declare
               C : Nodes.Node_Type;
            begin
               Read_Node(Tree, T, Nodes.Child(N, 1), C);
               Nodes.Set_Parent(C, Nodes.Invalid_Address);
               Write_Node(Tree, T, Nodes.Child(N, 1), C);
            end;
            T.Current_Root_Address := Nodes.Child(N, 1);
            Free_Node(Tree, T, N_A);
         else
            State := Error;
         end if;
      end Handle_Empty_Root;


      -- Simply delete.
      -- Since this terminates the path unwinding, the parent-synchronization 
      -- is performed recursively to the root node.
      procedure Direct_Delete
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         N_A   : in     Nodes.Valid_Address_Type;
         N     : in     Nodes.Node_Type;
         State : in out Result_Type)
      is begin
         if Nodes.Is_Valid(N) then
            Write_Node(Tree, T, N_A, N);
            Synchronize_With_Parent(Tree, T, N_A, N, State);
         else
            State := Error;
         end if;
      end Direct_Delete;


      -- Redistributes the entries of the given node and its right neighbor.
      -- Since this terminates the path unwinding, the parent-synchronization 
      -- is performed recursively to the root node. Since the right neighbor
      -- and the current node might have different parents, both nodes have
      -- to be synchronized recursively.
      procedure Shift_From_Right
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         N_A   : in     Nodes.Valid_Address_Type;
         N     : in     Nodes.Node_Type;
         State : in out Result_Type)
      is begin
         if Nodes.Is_Valid(Nodes.Right_Neighbor(N)) then
            declare
               R_A : constant Nodes.Valid_Address_Type
                   := Nodes.Valid_Right_Neighbor(N);
               R   : Nodes.Node_Type;
            begin
               Read_Node(Tree, T, R_A, R);
               Redistribute(Tree, T, N_A, N, R_A, R, State);
            end;
         else
            State := Error;
         end if;
      end Shift_From_Right;


      -- Redistributes the entries of the given node and its left neighbor.
      -- Since this terminates the path unwinding, the parent-synchronization 
      -- is performed recursively to the root node. Since the left neighbor
      -- and the current node might have different parents, both nodes have
      -- to be synchronized recursively.
      procedure Shift_From_Left
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         N_A   : in     Nodes.Valid_Address_Type;
         N     : in     Nodes.Node_Type;
         State : in out Result_Type)
      is begin
         if Nodes.Is_Valid(Nodes.Left_Neighbor(N)) then
            declare
               L_A : constant Nodes.Valid_Address_Type
                   := Nodes.Valid_Left_Neighbor(N);
               L   : Nodes.Node_Type;
            begin
               Read_Node(Tree, T, L_A, L);
               Redistribute(Tree, T, L_A, L, N_A, N, State);
            end;
         else
            State := Error;
         end if;
      end Shift_From_Left;


      -- Helper procedure that creates a new node that contains L's and R's
      -- entries. The new node is written into the place of R. The old
      -- entry of R in its parent is updated and the old entry of L in its
      -- parent is removed and the place formerly occupied by L is freed.
      -- The new node is synchronized with its parent.
      procedure Merge
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         L_A   : in     Nodes.Valid_Address_Type;
         L     : in     Nodes.Node_Type;
         R_A   : in     Nodes.Valid_Address_Type;
         R     : in     Nodes.Node_Type;
         State : in out Result_Type)
      is

         procedure Delete_From_Parent
           (Tree  : in out Tree_Type;
            T     : in out RW_Transaction_Type'Class;
            N_A   : in     Nodes.Valid_Address_Type;
            N     : in     Nodes.Node_Type;
            State : in out Result_Type)
         is begin -- Delete_From_Parent
            if Nodes.Is_Valid(Nodes.Parent(N)) then
               declare
                  P_A   : constant Nodes.Valid_Address_Type
                        := Nodes.Valid_Parent(N);
                  P_Old : Nodes.Node_Type;
                  I     : Nodes.Index_Type;
                  P_New : Nodes.Node_Type;
               begin
                  Read_Node(Tree, T, P_A, P_Old);
                  I     := Nodes.Child_Position(P_Old, N_A);
                  P_New := Nodes.Deletion(P_Old, I);
                  Handle_Underflow(Tree, T, P_A, P_New, State);
               end;
            end if;
         end Delete_From_Parent;

         N : constant Nodes.Node_Type := Nodes.Combination(L, R);
      begin -- Merge
         if Nodes.Is_Valid(N) then
            if Nodes.Is_Valid(Nodes.Left_Neighbor(L)) then
               declare
                  LL_A : constant Nodes.Valid_Address_Type
                       := Nodes.Valid_Left_Neighbor(L);
                  LL   : Nodes.Node_Type;
               begin
                  Read_Node(Tree, T, LL_A, LL);
                  Nodes.Set_Right_Neighbor(LL, R_A);
                  Write_Node(Tree, T, LL_A, LL);
               end;
            end if;
            if Nodes.Is_Inner(L) then
               for I in 1 .. Nodes.Degree(L) loop
                  declare
                     C_A : constant Nodes.Valid_Address_Type
                         := Nodes.Child(L, I);
                     C   : Nodes.Node_Type;
                  begin
                     Read_Node(Tree, T, C_A, C);
                     Nodes.Set_Parent(C, R_A);
                     Write_Node(Tree, T, C_A, C);
                  end;
               end loop;
            end if;
            Write_Node(Tree, T, R_A, N);
            Synchronize_With_Parent(Tree, T, R_A, N, State);
            -- previous synchronization might have modified L's parent
            declare
               LL : Nodes.Node_Type;
            begin
               Read_Node(Tree, T, L_A, LL);
               Delete_From_Parent(Tree, T, L_A, LL, State);
            end;
            Free_Node(Tree, T, L_A);
         else
            State := Error;
         end if;
      end Merge;


      -- Merges the left neighbor with the current node R.
      -- The main job is done by the Merge procedure.
      procedure Merge_With_Left
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         R_A   : in     Nodes.Valid_Address_Type;
         R     : in     Nodes.Node_Type;
         State : in out Result_Type)
      is begin -- Merge_With_Left
         if Nodes.Is_Valid(Nodes.Left_Neighbor(R)) then
            declare
               L_A : constant Nodes.Valid_Address_Type
                   := Nodes.Valid_Left_Neighbor(R);
               L   : Nodes.Node_Type;
            begin
               Read_Node(Tree, T, L_A, L);
               Merge(Tree, T, L_A, L, R_A, R, State);
            end;
         else
            State := Error;
         end if;
      end Merge_With_Left;


      -- Merges the right neighbor with the current node L.
      -- The main job is done by the Merge procedure.
      procedure Merge_With_Right
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         L_A   : in     Nodes.Valid_Address_Type;
         L     : in     Nodes.Node_Type;
         State : in out Result_Type)
      is begin -- Merge_With_Right
         if Nodes.Is_Valid(Nodes.Right_Neighbor(L)) then
            declare
               R_A : constant Nodes.Valid_Address_Type
                   := Nodes.Valid_Right_Neighbor(L);
               R   : Nodes.Node_Type;
            begin
               Read_Node(Tree, T, R_A, R);
               Merge(Tree, T, L_A, L, R_A, R, State);
            end;
         else
            State := Error;
         end if;
      end Merge_With_Right;

   begin -- Handle_Underflow
      if State /= Success then
         return;
      end if;

      State := Success;
      Handle_Empty_Root(Tree, T, N_A, N, State);
      if State = Success then
         return;
      end if;

      State := Success;
      Merge_With_Left(Tree, T, N_A, N, State);
      if State = Success then
         return;
      end if;

      State := Success;
      Merge_With_Right(Tree, T, N_A, N, State);
      if State = Success then
         return;
      end if;

      State := Success;
      Direct_Delete(Tree, T, N_A, N, State);
      if State = Success then
         return;
      end if;

      State := Success;
      Shift_From_Right(Tree, T, N_A, N, State);
      if State = Success then
         return;
      end if;

      State := Success;
      Shift_From_Left(Tree, T, N_A, N, State);
   end Handle_Underflow;

end Deletion;

