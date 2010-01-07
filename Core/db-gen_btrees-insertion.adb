-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.Gen_BTrees)
package body Insertion is

   procedure Insert
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      Position :    out Count_Type;
      State    :    out State_Type)
   is
      pragma Assert (Tree.Initialized);
      Transaction : RW_Transaction_Type := New_RW_Transaction(Tree);
   begin
      Start_Transaction(Tree, Transaction);
      Insert(Tree, Transaction, Key, Value, Position, State);
      if State = Success then
         Commit_Transaction(Tree, Transaction);
      else
         Abort_Transaction(Tree, Transaction);
      end if;
   exception
      when others =>
         Abort_Transaction(Tree, Transaction);
         raise;
   end Insert;


   procedure Insert
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      Position    :    out Count_Type;
      State       :    out State_Type)
   is
      use type IO.Blocks.Size_Type;
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      pragma Assert (Transaction.Started);
      pragma Assert (Key_Size_Bound(Key) <=
                     Max_Key_Size(Value_Size_Bound(Value)));

      N_A : Nodes.Valid_Address_Type;
      I   : Nodes.Index_Type;
   begin
      Position := 0;
      if Key_Size_Bound(Key) > Max_Key_Size(Value_Size_Bound(Value)) then
         State := Failure;
         return;
      end if;

      -- Search leaf, fill buffer. Correctly initialize N_A and I to the leaf
      -- node address and the position at which the (Key, Value) should be
      -- inserted.
      N_A := Transaction.Current_Root_Address;
      loop
         declare
            use type Nodes.Degree_Type;
            N : Nodes.Node_Type;
         begin
            Read_Node(Tree, Transaction, N_A, N);
            I := Nodes.Key_Position(N, Key);
            if not Nodes.Is_Valid(I) then
               if Nodes.Is_Inner(N) then
                  I := Nodes.Degree(N);
               else
                  I := Nodes.Degree(N) + 1;
               end if;
            --elsif Key = Nodes.Key(N, I) then
               --State := Failure;
               --return;
            end if;
            if Nodes.Is_Leaf(N) then
               Position := Position + Nodes.Count_Sum(N, I);
               exit;
            end if;
            Position := Position + Nodes.Count_Sum(N, I-1);
            N_A      := Nodes.Child(N, I);
         end;
      end loop;

      -- Insert in memory and then dispatch to add the node.
      declare
         N_Old : Nodes.Node_Type;
         N_New : Nodes.Node_Type;
      begin
         Read_Node(Tree, Transaction, N_A, N_Old);
         N_New := Nodes.Insertion(N_Old, I, Key, Value);
         State := Success;
         Handle_Overflow(Tree, Transaction, N_A, N_New, State);
      end;

   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Insert;


   -- Dispatches to the appropriate next operation:
   -- 1. simply insert, or
   -- 2. insert and shift the minimum to the left neighbor, or
   -- 3. insert and shift the maximum to the right neighbor, or
   -- 4. insert, split and insert the new node into the parent.
   procedure Handle_Overflow
     (Tree  : in out Tree_Type;
      T     : in out RW_Transaction_Type'Class;
      N_A   : in     Nodes.Valid_Address_Type;
      N     : in     Nodes.Node_Type;
      State : in out State_Type)
   is

      -- Simply inserts.
      -- Since this terminates the path unwinding, the synchronization of
      -- the counts is performed recursively to the root node.
      procedure Direct_Insert
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         N_A   : in     Nodes.Valid_Address_Type;
         N     : in     Nodes.Node_Type;
         State : in out State_Type) is
      begin
         if Nodes.Is_Valid(N) then
            Write_Node(Tree, T, N_A, N);
            Synchronize_With_Parent(Tree, T, N_A, N, State);
         else
            State := Error;
         end if;
      end Direct_Insert;


      -- Redistributes the entries of the given node and its right neighbor.
      -- Since this terminates the path unwinding, the parent-synchronization
      -- is performed recursively to the root node. Since the right neighbor
      -- and the current node might have different parents, both nodes have
      -- to be synchronized recursively.
      procedure Shift_To_Right
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         N_A   : in     Nodes.Valid_Address_Type;
         N     : in     Nodes.Node_Type;
         State : in out State_Type) is
      begin
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
      end Shift_To_Right;


      -- Redistributes the entries of the given node and its left neighbor.
      -- Since this terminates the path unwinding, the parent-synchronization
      -- is performed recursively to the root node. Since the left neighbor
      -- and the current node might have different parents, both nodes have
      -- to be synchronized recursively.
      procedure Shift_To_Left
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         N_A   : in     Nodes.Valid_Address_Type;
         N     : in     Nodes.Node_Type;
         State : in out State_Type) is
      begin
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
      end Shift_To_Left;


      -- Insert, split to N := (L, R), update N to R and insert L into N's
      -- parent.
      -- After the update of N to R, the count in the parent of R must be
      -- synchronized. However, this does not have to be done recursively,
      -- because L is inserted into R's parent, and as a part of this
      -- insertion, the count of L in the parent will be synchronized
      -- recursively, and this will also synchronize the count of the parent
      -- in the grandparent and all other nodes in the path up to the root.
      procedure Split
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         N_A   : in     Nodes.Valid_Address_Type;
         N     : in     Nodes.Node_Type;
         State : in out State_Type)
      is

         procedure Insert_Into_New_Root
           (Tree  : in out Tree_Type;
            T     : in out RW_Transaction_Type'Class;
            L_A   : in     Nodes.Valid_Address_Type;
            L     : in out Nodes.Node_Type;
            R_A   : in     Nodes.Valid_Address_Type;
            R     : in out Nodes.Node_Type;
            State : in out State_Type) is
         begin
            Allocate_Node(Tree, T, T.Current_Root_Address);
            Nodes.Set_Parent(L, T.Current_Root_Address);
            Nodes.Set_Parent(R, T.Current_Root_Address);
            Write_Node(Tree, T, L_A, L);
            Write_Node(Tree, T, R_A, R);
            declare
               Root : constant Nodes.Node_Type := Nodes.Insertion(
                        Node  => Nodes.Insertion(
                                    Node  => Nodes.Root_Node(Is_Leaf => False),
                                    Index => 1,
                                    Key   => Nodes.Key(L, Nodes.Degree(L)),
                                    Child => L_A,
                                    Count => Nodes.Count_Sum(L)
                                 ),
                        Index => 2,
                        Key   => Nodes.Key(R, Nodes.Degree(R)),
                        Child => R_A,
                        Count => Nodes.Count_Sum(R));
            begin -- Insert_Into_New_Root
               if Nodes.Is_Valid(Root) then
                  Write_Node(Tree, T, T.Current_Root_Address, Root);
               else
                  State := Error;
               end if;
            end;
         end Insert_Into_New_Root;

         procedure Insert_Into_Parent
           (Tree  : in out Tree_Type;
            T     : in out RW_Transaction_Type'Class;
            L_A   : in     Nodes.Valid_Address_Type;
            L     : in     Nodes.Node_Type;
            State : in out State_Type)
         is
            P_A   : constant Nodes.Valid_Address_Type
                  := Nodes.Valid_Parent(L);
            R_A   : constant Nodes.Valid_Address_Type
                  := Nodes.Valid_Right_Neighbor(L);
            P_Old : Nodes.Node_Type;
            Key   : Key_Type;
            I     : Nodes.Index_Type;
            Cnt   : Count_Type;
            P_New : Nodes.Node_Type;
         begin -- Insert_Into_Parent
            Read_Node(Tree, T, P_A, P_Old);
            Key   := Nodes.Key(L, Nodes.Degree(L));
            I     := Nodes.Child_Position(P_Old, R_A);
            Cnt   := Nodes.Count_Sum(L);
            P_New := Nodes.Insertion(Node  => P_Old,
                                     Index => I,
                                     Key   => Key,
                                     Child => L_A,
                                     Count => Cnt);
            Handle_Overflow(Tree, T, P_A, P_New, State);
         end Insert_Into_Parent;

         use type Nodes.Degree_Type;
         I   : constant Nodes.Valid_Index_Type := Nodes.Split_Position(N);
         L   : Nodes.Node_Type := Nodes.Copy(N, 1, I - 1);
         R   : Nodes.Node_Type := Nodes.Copy(N, I, Nodes.Degree(N));
         L_A : Nodes.Valid_Address_Type;
         R_A : Nodes.Valid_Address_Type renames N_A;
      begin -- Split
         if Nodes.Is_Valid(Node => L, Force_Non_Root => True) and
            Nodes.Is_Valid(Node => R, Force_Non_Root => True) then
            Allocate_Node(Tree, T, L_A);
            Nodes.Set_Right_Neighbor(L, R_A);
            Nodes.Set_Left_Neighbor(R, L_A);
            if Nodes.Is_Inner(L) then
               for I in 1 .. Nodes.Degree(L) loop
                  declare
                     C_A : constant Nodes.Valid_Address_Type
                         := Nodes.Child(L, I);
                     C   : Nodes.Node_Type;
                  begin
                     Read_Node(Tree, T, C_A, C);
                     Nodes.Set_Parent(C, L_A);
                     Write_Node(Tree, T, C_A, C);
                  end;
               end loop;
            end if;
            Write_Node(Tree, T, L_A, L);
            Write_Node(Tree, T, R_A, R);
            if Nodes.Is_Valid(Nodes.Left_Neighbor(L)) then
               declare
                  LL_A : constant Nodes.Valid_Address_Type
                       := Nodes.Valid_Left_Neighbor(L);
                  LL   : Nodes.Node_Type;
               begin
                  Read_Node(Tree, T, LL_A, LL);
                  Nodes.Set_Right_Neighbor(LL, L_A);
                  Write_Node(Tree, T, LL_A, LL);
               end;
            end if;
            if Nodes.Is_Root(N) then
               Insert_Into_New_Root(Tree, T, L_A, L, R_A, R, State);
            else
               Insert_Into_Parent(Tree, T, L_A, L, State);
               -- previous insertion might have modified L's parent
               Read_Node(Tree, T, R_A, R);
               Synchronize_With_Parent(Tree, T, R_A, R, State);
            end if;
         else
            State := Error;
         end if;
      end Split;

   begin -- Handle_Overflow
      if State /= Success then
         return;
      end if;

      Direct_Insert(Tree, T, N_A, N, State);
      if State = Success then
         return;
      end if;

      State := Success;
      Shift_To_Left(Tree, T, N_A, N, State);
      if State = Success then
         return;
      end if;

      State := Success;
      Shift_To_Right(Tree, T, N_A, N, State);
      if State = Success then
         return;
      end if;

      State := Success;
      Split(Tree, T, N_A, N, State);
   end Handle_Overflow;

end Insertion;

