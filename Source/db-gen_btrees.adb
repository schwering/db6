package body DB.Gen_BTrees is

   package Initialization is
      procedure Create
        (ID : in  String);

      procedure Initialize
        (Tree : out Tree_Type;
         ID   : in  String);

      procedure Finalize
        (Tree : in out Tree_Type);
   end Initialization;


   package Search is
      procedure Look_Up
        (Tree     : in out Tree_Type;
         Key      : in     Key_Type;
         Value    :    out Value_Type;
         Position :    out Count_Type;
         State    :    out Result_Type);

      procedure Look_Up
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Key         : in     Key_Type;
         Value       :    out Value_Type;
         Position    :    out Count_Type;
         State       :    out Result_Type);

      procedure Look_Up
        (Tree     : in out Tree_Type;
         Position : in     Count_Type;
         Value    :    out Value_Type;
         Key      :    out Key_Type;
         State    :    out Result_Type);

      procedure Look_Up
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Position    : in     Count_Type;
         Value       :    out Value_Type;
         Key         :    out Key_Type;
         State       :    out Result_Type);

      procedure Minimum
        (Tree     : in out Tree_Type;
         Key      :    out Key_Type;
         Value    :    out Value_Type;
         Position :    out Count_Type;
         State    :    out Result_Type);

      procedure Minimum
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Key         :    out Key_Type;
         Value       :    out Value_Type;
         Position    :    out Count_Type;
         State       :    out Result_Type);

      procedure Maximum
        (Tree     : in out Tree_Type;
         Key      :    out Key_Type;
         Value    :    out Value_Type;
         Position :    out Count_Type;
         State    :    out Result_Type);

      procedure Maximum
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Key         :    out Key_Type;
         Value       :    out Value_Type;
         Position    :    out Count_Type;
         State       :    out Result_Type);
   end Search;


   package Insertion is
      procedure Insert
        (Tree     : in out Tree_Type;
         Key      : in     Key_Type;
         Value    : in     Value_Type;
         Position :    out Count_Type;
         State    :    out Result_Type);

      procedure Insert
        (Tree        : in out Tree_Type;
         Transaction : in out RW_Transaction_Type'Class;
         Key         : in     Key_Type;
         Value       : in     Value_Type;
         Position    :    out Count_Type;
         State       :    out Result_Type);

      procedure Handle_Overflow
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         N_A   : in     Nodes.Valid_Address_Type;
         N     : in     Nodes.Node_Type;
         State : in out Result_Type);
   end Insertion;


   package Deletion is
      procedure Delete
        (Tree     : in out Tree_Type;
         Key      : in     Key_Type;
         Value    :    out Value_Type;
         Position :    out Count_Type;
         State    :    out Result_Type);

      procedure Delete
        (Tree        : in out Tree_Type;
         Transaction : in out RW_Transaction_Type'Class;
         Key         : in     Key_Type;
         Value       :    out Value_Type;
         Position    :    out Count_Type;
         State       :    out Result_Type);

      procedure Delete
        (Tree     : in out Tree_Type;
         Position : in     Count_Type;
         Value    :    out Value_Type;
         Key      :    out Key_Type;
         State    :    out Result_Type);

      procedure Delete
        (Tree        : in out Tree_Type;
         Transaction : in out RW_Transaction_Type'Class;
         Position    : in     Count_Type;
         Value       :    out Value_Type;
         Key         :    out Key_Type;
         State       :    out Result_Type);

      procedure Handle_Underflow
        (Tree  : in out Tree_Type;
         T     : in out RW_Transaction_Type'Class;
         N_A   : in     Nodes.Valid_Address_Type;
         N     : in     Nodes.Node_Type;
         State : in out Result_Type);
   end Deletion;


   package Cursors is
      function Positive_Infinity_Bound
         return Bound_Type;

      function Negative_Infinity_Bound
         return Bound_Type;

      function New_Bound
        (Comparison : Comparison_Type;
         Key        : Key_Type)
         return Bound_Type;

      function New_Cursor
        (Tree              : Tree_Type;
         Transaction       : Transaction_Type'Class;
         Thread_Safe       : Boolean;
         Lower_Bound       : Bound_Type;
         Upper_Bound       : Bound_Type;
         Reverse_Direction : Boolean := False)
         return Cursor_Type;

      procedure Set_Thread_Safety
        (Cursor  : in out Cursor_Type;
         Enabled : in     Boolean);

      procedure Finalize
        (Tree   : in out Tree_Type;
         Cursor : in out Cursor_Type);

      procedure Pause
        (Tree   : in out Tree_Type;
         Cursor : in out Cursor_Type);

      procedure Unpause
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Cursor      : in out Cursor_Type);

      procedure Next
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Cursor      : in out Cursor_Type;
         Key         :    out Key_Type;
         Value       :    out Value_Type;
         State       :    out Result_Type);

      procedure Delete
        (Tree        : in out Tree_Type;
         Transaction : in out RO_Transaction_Type'Class;
         Cursor      : in out Cursor_Type;
         State       :    out Result_Type);

      procedure Delete
        (Tree        : in out Tree_Type;
         Transaction : in out RW_Transaction_Type'Class;
         Cursor      : in out Cursor_Type;
         State       :    out Result_Type);
   end Cursors;


   package Misc is
      procedure Count
        (Tree  : in out Tree_Type;
         Count :    out Count_Type);

      procedure Count
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Count       :    out Count_Type);

      procedure Get_Height
        (Tree   : in out Tree_Type;
         Height :    out Height_Type);

      procedure Get_Height
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Height      :    out Height_Type);

      procedure Clusterize
        (Tree  : in out Tree_Type;
         State :    out Result_Type);
   end Misc;


   procedure Read_Node
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type;
      N    :    out Nodes.Node_Type)
   is
      pragma Inline (Read_Node);
      Block : IO.Blocks.Block_Type;
   begin
      Block_IO.Read(Tree.File, Block_IO.Valid_Address_Type(N_A), Block);
      N := Nodes.From_Block(Block);
   end Read_Node;


   procedure Read_Node
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           :    out Nodes.Node_Type)
   is begin
      raise Tree_Error;
   end Read_Node;


   procedure Read_Node
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           :    out Nodes.Node_Type)
   is
      pragma Warnings (Off, Transaction);
      pragma Inline (Read_Node);
   begin
      Read_Node(Tree, N_A, N);
   end Read_Node;


   procedure Read_Node
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           :    out Nodes.Node_Type)
   is
      pragma Inline (Read_Node);
   begin
      IO_Buffers.Read(Tree.File, Transaction.Buffer,
                      Block_IO.Valid_Address_Type(N_A), N);
   end Read_Node;


   procedure Write_Node
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           : in     Nodes.Node_Type)
   is
      pragma Inline (Write_Node);
   begin
      IO_Buffers.Write(Tree.File, Transaction.Buffer,
                       Block_IO.Valid_Address_Type(N_A), N);
   end Write_Node;


   procedure Swap_Nodes
     (Tree : in out Tree_Type;
      T    : in out RW_Transaction_Type'Class;
      M_A  : in     Nodes.Valid_Address_Type;
      N_A  : in     Nodes.Valid_Address_Type)
   is
      procedure Prepare_References
        (Tree : in out Tree_Type;
         T    : in out RW_Transaction_Type'Class;
         N_A  : in     Nodes.Valid_Address_Type;
         N    : in     Nodes.Node_Type)
      is
         P_A : constant Nodes.Address_Type := Nodes.Parent(N);
         L_A : constant Nodes.Address_Type := Nodes.Left_Neighbor(N);
         R_A : constant Nodes.Address_Type := Nodes.Right_Neighbor(N);
      begin
         if Nodes.Is_Valid(P_A) then
            declare
               P   : Nodes.Node_Type;
               Key : Key_Type;
               I   : Nodes.Index_Type;
               Cnt : Count_Type;
            begin
               Read_Node(Tree, T, Nodes.To_Valid_Address(P_A), P);
               Key := Nodes.Key(N, Nodes.Degree(N));
               I   := Nodes.Key_Position(P, Key);
               Cnt := Nodes.Count(P, I);
               Nodes.Set_Child_And_Count(P, I, N_A, Cnt);
               Write_Node(Tree, T, Nodes.To_Valid_Address(P_A), P);
            end;
         end if;
         if Nodes.Is_Valid(L_A) then
            declare
               L : Nodes.Node_Type;
            begin
               Read_Node(Tree, T, Nodes.To_Valid_Address(L_A), L);
               Nodes.Set_Right_Neighbor(L, N_A);
               Write_Node(Tree, T, Nodes.To_Valid_Address(L_A), L);
            end;
         end if;
         if Nodes.Is_Valid(R_A) then
            declare
               R : Nodes.Node_Type;
            begin
               Read_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
               Nodes.Set_Left_Neighbor(R, N_A);
               Write_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
            end;
         end if;
         if Nodes.Is_Inner(N) then
            for I in 1 .. Nodes.Degree(N) loop
               declare
                  C_A : constant Nodes.Valid_Address_Type := Nodes.Child(N, I);
                  C   : Nodes.Node_Type;
               begin
                  Read_Node(Tree, T, C_A, C);
                  Nodes.Set_Parent(C, N_A);
                  Write_Node(Tree, T, C_A, C);
               end;
            end loop;
         end if;
      end Prepare_References;

      procedure Prepare_Free_References
        (Tree : in out Tree_Type;
         T    : in out RW_Transaction_Type'Class;
         N_A  : in     Nodes.Valid_Address_Type;
         N    : in     Nodes.Node_Type)
      is
         L_A : constant Nodes.Address_Type := Nodes.Left_Neighbor(N);
         R_A : constant Nodes.Address_Type := Nodes.Right_Neighbor(N);
      begin
         if Nodes.Is_Valid(L_A) then
            declare
               L : Nodes.Node_Type;
            begin
               Read_Node(Tree, T, Nodes.To_Valid_Address(L_A), L);
               Nodes.Set_Right_Neighbor(L, N_A);
               Write_Node(Tree, T, Nodes.To_Valid_Address(L_A), L);
            end;
         end if;
         if Nodes.Is_Valid(R_A) then
            declare
               R : Nodes.Node_Type;
            begin
               Read_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
               Nodes.Set_Left_Neighbor(R, N_A);
               Write_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
            end;
         end if;
      end Prepare_Free_References;

      use type Nodes.Valid_Address_Type;
   begin
      if N_A = M_A then
         return;
      end if;

      declare
         M : Nodes.Node_Type;
         N : Nodes.Node_Type;
      begin
         Read_Node(Tree, T, M_A, M);
         Read_Node(Tree, T, N_A, N);
         if not Nodes.Is_Free(M) then
            Prepare_References(Tree, T, N_A, M);
         else
            Prepare_Free_References(Tree, T, N_A, M);
         end if;
         if not Nodes.Is_Free(N) then
            Prepare_References(Tree, T, M_A, N);
         else
            Prepare_Free_References(Tree, T, M_A, N);
         end if;
      end;
      -- Nodes have to be re-read before being written to their destination.
      declare
         M : Nodes.Node_Type;
         N : Nodes.Node_Type;
      begin
         Read_Node(Tree, T, M_A, M);
         Read_Node(Tree, T, N_A, N);
         Write_Node(Tree, T, N_A, M);
         Write_Node(Tree, T, M_A, N);
      end;
   end Swap_Nodes;


   function New_RO_Transaction
     (Tree : Tree_Type)
      return RO_Transaction_Type
   is
      pragma Assert (Tree.Initialized);
   begin
      return (Owning_Tree => Tree.Self,
              Initialized => True,
              others      => <>);
   end New_RO_Transaction;


   procedure Start_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      pragma Assert (not Transaction.Started);
   begin
      Transaction.Started := True;
      Block_IO.Acquire_Ticket(Tree.File, Transaction.Ticket);
      Block_IO.Read_Lock(Tree.File, Transaction.Ticket);
   end Start_Transaction;


   procedure Finish_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
   begin
      if Transaction.Started then
         Block_IO.Unlock(Tree.File, Transaction.Ticket);
         Block_IO.Release_Ticket(Tree.File, Transaction.Ticket);
         Transaction.Started := False;
      end if;
      Transaction.Initialized := False;
   end Finish_Transaction;


   function New_RW_Transaction
     (Tree : Tree_Type)
      return RW_Transaction_Type
   is
      pragma Assert (Tree.Initialized);
   begin
      return (Owning_Tree => Tree.Self,
              Initialized => True,
              others      => <>);
   end New_RW_Transaction;


   procedure Start_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      pragma Assert (not Transaction.Started);
   begin
      Transaction.Current_Root_Address := Root_Address;
      Transaction.Buffer               := IO_Buffers.New_Buffer;
      Transaction.Started              := True;
      Block_IO.Acquire_Ticket(Tree.File, Transaction.Ticket);
      Block_IO.Write_Lock(Tree.File, Transaction.Ticket);
   end Start_Transaction;


   procedure Abort_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
   begin
      if Transaction.Started then
         Block_IO.Unlock(Tree.File, Transaction.Ticket);
         Block_IO.Release_Ticket(Tree.File, Transaction.Ticket);
         IO_Buffers.Free(Transaction.Buffer);
         Transaction.Started := False;
      end if;
      Transaction.Initialized := False;
   end Abort_Transaction;


   procedure Commit_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      pragma Assert (Transaction.Started);
      use type Nodes.Valid_Address_Type;
   begin
      if Transaction.Current_Root_Address /= Root_Address then
         Swap_Nodes(Tree, Transaction, Root_Address,
                    Transaction.Current_Root_Address);
         Transaction.Current_Root_Address := Root_Address;
      end if;
      Block_IO.Certify_Lock(Tree.File, Transaction.Ticket);
      IO_Buffers.Commit(Tree.File, Transaction.Buffer);
      Block_IO.Unlock(Tree.File, Transaction.Ticket);
      Block_IO.Release_Ticket(Tree.File, Transaction.Ticket);
      IO_Buffers.Free(Transaction.Buffer);
      Transaction.Started     := False;
      Transaction.Initialized := False;
   end Commit_Transaction;


   function New_RW_Transaction
     (Tree : Tree_Type)
      return Sub_RW_Transaction_Type
   is begin
      raise Tree_Error;
      pragma Warnings (Off);
      return (RW_Transaction_Type with others => <>);
      pragma Warnings (On);
   end New_RW_Transaction;


   function New_Sub_RW_Transaction
     (Tree               : Tree_Type;
      Owning_Transaction : RO_Transaction_Type'Class)
      return Sub_RW_Transaction_Type
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Owning_Transaction.Owning_Tree = Tree.Self);
   begin
      return (Owning_Tree        => Tree.Self,
              Initialized        => True,
              Owning_Transaction => Owning_Transaction.Self,
              others             => <>);
   end New_Sub_RW_Transaction;


   overriding
   procedure Start_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out Sub_RW_Transaction_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      pragma Assert (not Transaction.Started);
   begin
      Transaction.Current_Root_Address := Root_Address;
      Transaction.Buffer               := IO_Buffers.New_Buffer;
      Transaction.Started              := True;
      Transaction.Ticket               := Transaction.Owning_Transaction.Ticket;
      Block_IO.Write_Lock(Tree.File, Transaction.Ticket);
   end Start_Transaction;


   overriding
   procedure Abort_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out Sub_RW_Transaction_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
   begin
      if Transaction.Started then
         Block_IO.Read_Lock(Tree.File, Transaction.Ticket);
         IO_Buffers.Free(Transaction.Buffer);
         Transaction.Started := False;
      end if;
      Transaction.Initialized := False;
   end Abort_Transaction;


   overriding
   procedure Commit_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out Sub_RW_Transaction_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      pragma Assert (Transaction.Started);
      use type Nodes.Valid_Address_Type;
   begin
      if Transaction.Current_Root_Address /= Root_Address then
         Swap_Nodes(Tree, Transaction, Root_Address,
                    Transaction.Current_Root_Address);
         Transaction.Current_Root_Address := Root_Address;
      end if;
      Block_IO.Certify_Lock(Tree.File, Transaction.Ticket);
      IO_Buffers.Commit(Tree.File, Transaction.Buffer);
      Block_IO.Read_Lock(Tree.File, Transaction.Ticket);
      IO_Buffers.Free(Transaction.Buffer);
      Transaction.Started     := False;
      Transaction.Initialized := False;
   end Commit_Transaction;


   -- Allocates a new node by firstly checking the list of free blocks to
   -- whose beginning Tree_Type.Free_Head points. If this list is empty, a new
   -- block at the end of the file is allocated (in fact, the address is
   -- calculated; the block allocated when the buffer is written to disk).
   procedure Allocate_Node
     (Tree    : in out Tree_Type;
      T       : in out RW_Transaction_Type'Class;
      Address :    out Nodes.Valid_Address_Type)
   is
      F_A : Nodes.Valid_Address_Type renames Free_Address;
      F   : Nodes.Node_Type;
   begin
      Read_Node(Tree, T, F_A, F);
      if Nodes.Is_Valid(Nodes.Right_Neighbor(F)) then
         declare
            N_A : constant Nodes.Valid_Address_Type
                := Nodes.To_Valid_Address(Nodes.Right_Neighbor(F));
            N   : Nodes.Node_Type;
            R_A : Nodes.Address_Type;
         begin
            Read_Node(Tree, T, N_A, N);
            R_A := Nodes.Right_Neighbor(N);
            Nodes.Set_Right_Neighbor(F, R_A);
            Write_Node(Tree, T, F_A, F);
            if Nodes.Is_Valid(R_A) then
               declare
                  R : Nodes.Node_Type;
               begin
                  Read_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
                  Nodes.Set_Left_Neighbor(R, F_A);
                  Write_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
               end;
            end if;
            Address := N_A;
         end;
      else
         IO_Buffers.Seek_New(Tree.File, T.Buffer,
                             Block_IO.Valid_Address_Type(Address));
      end if;
   end Allocate_Node;


   -- Frees a block by prepending it to the list of emtpy blocks to whose
   -- beginning Tree_Type.Free_Head points.
   procedure Free_Node
     (Tree    : in out Tree_Type;
      T       : in out RW_Transaction_Type'Class;
      Address : in     Nodes.Valid_Address_Type)
   is
      F_A : Nodes.Valid_Address_Type renames Free_Address;
      F   : Nodes.Node_Type;
      N_A : Nodes.Valid_Address_Type renames Address;
      N   : Nodes.Node_Type := Nodes.Free_Node;
      R_A : Nodes.Address_Type;
   begin
      Read_Node(Tree, T, F_A, F);
      R_A := Nodes.Right_Neighbor(F);
      Nodes.Set_Right_Neighbor(F, N_A);
      Write_Node(Tree, T, F_A, F);
      Nodes.Set_Left_Neighbor(N, F_A);
      Nodes.Set_Right_Neighbor(N, R_A);
      Write_Node(Tree, T, N_A, N);
      if Nodes.Is_Valid(R_A) then
         declare
            R : Nodes.Node_Type;
         begin
            Read_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
            Nodes.Set_Left_Neighbor(R, N_A);
            Write_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
         end;
      end if;
   end Free_Node;


   -- Synchronizes the count and the key entries of N's parent with the actual
   -- count respectively with the greatest key in N.
   -- The procedure steps up recursively if either the count has really
   -- changed or if the key has really changed and the entry was last one in N.
   procedure Synchronize_With_Parent
     (Tree  : in out Tree_Type;
      T     : in out RW_Transaction_Type'Class;
      N_A   : in     Nodes.Valid_Address_Type;
      N     : in     Nodes.Node_Type;
      State : in out Result_Type)
   is begin
      if Nodes.Is_Valid(Nodes.Parent(N)) then
         declare
            use type Nodes.Degree_Type;
            use type Count_Type;
            P_A       : constant Nodes.Valid_Address_Type
                      := Nodes.Valid_Parent(N);
            P         : Nodes.Node_Type;
            I         : Nodes.Valid_Index_Type;
            Old_Key   : Key_Type;
            New_Key   : Key_Type;
            Old_Count : Count_Type;
            New_Count : Count_Type;
            New_P     : Nodes.Node_Type;
         begin
            Read_Node(Tree, T, P_A, P);
            I := Nodes.Child_Position(P, N_A);
            Old_Key := Nodes.Key(P, I);
            New_Key := Nodes.Key(N, Nodes.Degree(N));
            Old_Count := Nodes.Count(P, I);
            New_Count := Nodes.Count_Sum(N);
            New_P := Nodes.Substitution(P, I, New_Key, N_A, New_Count);
            case Nodes.Validation(New_P) is
               when Nodes.Valid =>
                  Write_Node(Tree, T, P_A, New_P);
                  if (I = Nodes.Degree(New_P) and Old_Key /= New_Key) or
                     Old_Count /= New_Count then
                     Synchronize_With_Parent(Tree, T, P_A, New_P, State);
                  end if;
               when Nodes.Too_Small =>
                  Deletion.Handle_Underflow(Tree, T, P_A, New_P, State);
               when Nodes.Too_Large =>
                  Insertion.Handle_Overflow(Tree, T, P_A, New_P, State);
            end case;
         end;
      end if;
   end Synchronize_With_Parent;


   -- Redistribute the entries of two neighbors. This is analoguous to
   -- shift in standard B-Trees.
   -- Since this terminates the path unwinding, the parent-synchronization 
   -- is performed recursively to the root node. Since the right neighbor
   -- and the current node might have different parents, both nodes have
   -- to be synchronized recursively.
   procedure Redistribute
     (Tree  : in out Tree_Type;
      T     : in out RW_Transaction_Type'Class;
      L_A   : in     Nodes.Valid_Address_Type;
      L     : in     Nodes.Node_Type;
      R_A   : in     Nodes.Valid_Address_Type;
      R     : in     Nodes.Node_Type;
      State : in out Result_Type)
   is
      function Combi_Child
        (Left_Node  : Nodes.Node_Type;
         Right_Node : Nodes.Node_Type;
         Index      : Nodes.Valid_Index_Type)
         return Nodes.Valid_Address_Type
      is
         use type Nodes.Valid_Index_Type;
      begin
         if Index <= Nodes.Degree(Left_Node) then
            return Nodes.Child(Left_Node, Index);
         else
            return Nodes.Child(Right_Node, Index - Nodes.Degree(Left_Node));
         end if;
      end Combi_Child;

      use type Nodes.Degree_Type;
      use type Nodes.Valid_Address_Type;
      --N : constant Nodes.Node_Type := Nodes.Combination(L, R);
      I     : constant Nodes.Valid_Index_Type
            := Nodes.Combi_Split_Position(L, R);
      L_New : Nodes.Node_Type
            := Nodes.Combi_Copy(L, R, 1, I-1);
      R_New : Nodes.Node_Type
            := Nodes.Combi_Copy(L, R, I, Nodes.Degree(L) + Nodes.Degree(R));
   begin
      if Nodes.Is_Valid(L_New, Force_Non_Root => True) and
         Nodes.Is_Valid(R_New, Force_Non_Root => True) then
         Nodes.Set_Parent(L_New, Nodes.Parent(L));
         Nodes.Set_Right_Neighbor(L_New, R_A);

         Nodes.Set_Parent(R_New, Nodes.Parent(R));
         Nodes.Set_Left_Neighbor(R_New, L_A);

         if Nodes.Valid_Right_Neighbor(L) /= R_A then
            raise Tree_Error;
         end if;

         if Nodes.Is_Inner(L) then
            -- those moved from left to the right
            for K in I .. Nodes.Degree(L) loop
               declare
                  C_A : constant Nodes.Valid_Address_Type
                      := Combi_Child(L, R, K);
                  C   : Nodes.Node_Type;
               begin
                  Read_Node(Tree, T, C_A, C);
                  Nodes.Set_Parent(C, R_A);
                  Write_Node(Tree, T, C_A, C);
               end;
            end loop;
            -- those moved from right to the left
            for K in Nodes.Degree(L) + 1 .. I - 1 loop
               declare
                  C_A : constant Nodes.Valid_Address_Type
                      := Combi_Child(L, R, K);
                  C   : Nodes.Node_Type;
               begin
                  Read_Node(Tree, T, C_A, C);
                  Nodes.Set_Parent(C, L_A);
                  Write_Node(Tree, T, C_A, C);
               end;
            end loop;
         end if;
         Write_Node(Tree, T, L_A, L_New);
         Write_Node(Tree, T, R_A, R_New);
         Synchronize_With_Parent(Tree, T, L_A, L_New, State);
         -- previous synchronization might have modified R_New's parent
         Read_Node(Tree, T, R_A, R_New);
         Synchronize_With_Parent(Tree, T, R_A, R_New, State);
      else
         State := Error;
      end if;
   end Redistribute;


   procedure Create
     (ID : in String)
   renames Initialization.Create;


   procedure Initialize
     (Tree : out Tree_Type;
      ID   : in  String)
   renames Initialization.Initialize;


   procedure Finalize
     (Tree : in out Tree_Type)
   renames Initialization.Finalize;


   function Max_Key_Size
     (Max_Value_Size : IO.Blocks.Size_Type
                     := IO.Blocks.Bits_To_Units(Value_Type'Size))
      return IO.Blocks.Size_Type
   renames Nodes.Max_Key_Size;


   procedure Look_Up
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   renames Search.Look_Up;


   procedure Look_Up
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type)
   renames Search.Look_Up;


   procedure Look_Up
     (Tree     : in out Tree_Type;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type)
   renames Search.Look_Up;


   procedure Look_Up
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   renames Search.Look_Up;


   procedure Minimum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   renames Search.Minimum;


   procedure Minimum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   renames Search.Minimum;


   procedure Maximum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   renames Search.Maximum;


   procedure Maximum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   renames Search.Maximum;


   procedure Insert
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   renames Insertion.Insert;


   procedure Insert
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   renames Insertion.Insert;


   procedure Delete
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   renames Deletion.Delete;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   renames Deletion.Delete;


   procedure Delete
     (Tree     : in out Tree_Type;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type)
   renames Deletion.Delete;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type)
   renames Deletion.Delete;


   function Positive_Infinity_Bound
      return Bound_Type
   renames Cursors.Positive_Infinity_Bound;


   function Negative_Infinity_Bound
      return Bound_Type
   renames Cursors.Negative_Infinity_Bound;


   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type
   renames Cursors.New_Bound;


   function New_Cursor
     (Tree              : Tree_Type;
      Transaction       : Transaction_Type'Class;
      Thread_Safe       : Boolean;
      Lower_Bound       : Bound_Type;
      Upper_Bound       : Bound_Type;
      Reverse_Direction : Boolean := False)
      return Cursor_Type
   renames Cursors.New_Cursor;

   
   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean)
   renames Cursors.Set_Thread_Safety;


   procedure Finalize
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type)
   renames Cursors.Finalize;


   procedure Pause
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type)
   renames Cursors.Pause;


   procedure Unpause
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type)
   renames Cursors.Unpause;


   procedure Next
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out Result_Type)
   renames Cursors.Next;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      State       :    out Result_Type)
   renames Cursors.Delete;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      State       :    out Result_Type)
   renames Cursors.Delete;


   procedure Count
     (Tree  : in out Tree_Type;
      Count :    out Count_Type)
   renames Misc.Count;


   procedure Count
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Count       :    out Count_Type)
   renames Misc.Count;


   procedure Get_Height
     (Tree   : in out Tree_Type;
      Height :    out Height_Type)
   renames Misc.Get_Height;


   procedure Get_Height
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Height_Type)
   renames Misc.Get_Height;


   procedure Clusterize
     (Tree  : in out Tree_Type;
      State :    out Result_Type)
   renames Misc.Clusterize;


   package body Nodes is separate;
   package body Initialization is separate;
   package body Search is separate;
   package body Insertion is separate;
   package body Deletion is separate;
   package body Cursors is separate;
   package body Misc is separate;

end DB.Gen_BTrees;
 
