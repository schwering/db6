-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

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


   package Retrieval is
      procedure Retrieve
        (Tree     : in out Tree_Type;
         Key      : in     Key_Type;
         Value    :    out Value_Type;
         State    :    out State_Type);

      procedure Minimum
        (Tree     : in out Tree_Type;
         Key      :    out Key_Type;
         Value    :    out Value_Type;
         State    :    out State_Type);

      procedure Maximum
        (Tree     : in out Tree_Type;
         Key      :    out Key_Type;
         Value    :    out Value_Type;
         State    :    out State_Type);
   end Retrieval;


   package Insertion is
      procedure Insert
        (Tree     : in out Tree_Type;
         Key      : in     Key_Type;
         Value    : in     Value_Type;
         State    :    out State_Type);
   end Insertion;


   package Deletion is
      procedure Delete
        (Tree     : in out Tree_Type;
         Key      : in     Key_Type;
         Value    :    out Value_Type;
         State    :    out State_Type);
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
         Thread_Safe       : Boolean;
         Lower_Bound       : Bound_Type;
         Upper_Bound       : Bound_Type;
         Reverse_Direction : Boolean := False)
         return Cursor_Type;

      procedure Set_Thread_Safety
        (Cursor  : in out Cursor_Type;
         Enabled : in     Boolean);

      procedure Finalize_Cursor
        (Tree   : in     Tree_Type;
         Cursor : in out Cursor_Type);

      procedure Pause
        (Tree   : in out Tree_Type;
         Cursor : in out Cursor_Type);

      procedure Next
        (Tree   : in out Tree_Type;
         Cursor : in out Cursor_Type;
         Key    :    out Key_Type;
         Value  :    out Value_Type;
         State  :    out State_Type);

      procedure Delete
        (Tree   : in out Tree_Type;
         Cursor : in out Cursor_Type;
         Key    :    out Key_Type;
         Value  :    out Value_Type;
         State  :    out State_Type);
   end Cursors;


   package Misc is
      procedure Count
        (Tree  : in out Tree_Type;
         Count :    out Count_Type);

      procedure Get_Height
        (Tree   : in out Tree_Type;
         Height :    out Natural);

      procedure Reorganize
        (Tree  : in out Tree_Type;
         State :    out State_Type);
   end Misc;


   function "<="
     (Left, Right : Key_Type)
      return Boolean
   is
      use type Utils.Comparison_Result_Type;
      C : constant Utils.Comparison_Result_Type := Compare(Left, Right);
   begin
      return C = Utils.Less or C = Utils.Equal;
   end "<=";


   function "="
     (Left, Right : Key_Type)
      return Boolean
   is
      use type Utils.Comparison_Result_Type;
      C : constant Utils.Comparison_Result_Type := Compare(Left, Right);
   begin
      return C = Utils.Equal;
   end "=";


   procedure Read_Node
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type;
      N    :    out Nodes.Node_Type)
   is
      pragma Inline (Read_Node);
   begin
      Block_IO.Read(Tree.File, Block_IO.Valid_Address_Type(N_A),
                    IO.Blocks.Base_Block_Type(N(Nodes.RO_Node_Type'Range)));
   end Read_Node;


   procedure Write_Node
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type;
      N    : in     Nodes.RW_Node_Type)
   is
      pragma Inline (Write_Node);
      use type Nodes.Validation_State_Type;
      pragma Assert (Nodes.Validation(N) = Nodes.Valid);
   begin
      Block_IO.Write(Tree.File, Block_IO.Valid_Address_Type(N_A),
                     Nodes.To_Block(N));
   end Write_Node;


   procedure Swap_Nodes
     (Tree : in out Tree_Type;
      T    : in out RW_Transaction_Type'Class;
      M_A  : in     Nodes.Valid_Address_Type;
      N_A  : in     Nodes.Valid_Address_Type)
   is
      procedure Prepare_References
        (Tree    : in out Tree_Type;
         T       : in out RW_Transaction_Type'Class;
         N       : in     Nodes.RW_Node_Type;
         Old_N_A : in     Nodes.Valid_Address_Type;
         New_N_A : in     Nodes.Valid_Address_Type)
      is
         P_A : constant Nodes.Address_Type := Nodes.Parent(N);
         L_A : constant Nodes.Address_Type := Nodes.Left_Neighbor(N);
         R_A : constant Nodes.Address_Type := Nodes.Right_Neighbor(N);
      begin
         if Nodes.Is_Valid(P_A) then
            declare
               P   : Nodes.RW_Node_Type;
               I   : Nodes.Index_Type;
            begin
               Read_Node(Tree, T, Nodes.To_Valid_Address(P_A), P);
               I := Nodes.Child_Position(P, Old_N_A);
               Nodes.Set_Child(P, I, New_N_A);
               Write_Node(Tree, T, Nodes.To_Valid_Address(P_A), P);
            end;
         end if;
         if Nodes.Is_Valid(L_A) then
            declare
               L : Nodes.RW_Node_Type;
            begin
               Read_Node(Tree, T, Nodes.To_Valid_Address(L_A), L);
               Nodes.Set_Right_Neighbor(L, New_N_A);
               Write_Node(Tree, T, Nodes.To_Valid_Address(L_A), L);
            end;
         end if;
         if Nodes.Is_Valid(R_A) then
            declare
               R : Nodes.RW_Node_Type;
            begin
               Read_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
               Nodes.Set_Left_Neighbor(R, New_N_A);
               Write_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
            end;
         end if;
         if Nodes.Is_Inner(N) then
            for I in 1 .. Nodes.Degree(N) loop
               declare
                  C_A : constant Nodes.Valid_Address_Type := Nodes.Child(N, I);
                  C   : Nodes.RW_Node_Type;
               begin
                  Read_Node(Tree, T, C_A, C);
                  Nodes.Set_Parent(C, New_N_A);
                  Write_Node(Tree, T, C_A, C);
               end;
            end loop;
         end if;
      end Prepare_References;

      procedure Prepare_Free_References
        (Tree    : in out Tree_Type;
         T       : in out RW_Transaction_Type'Class;
         N       : in     Nodes.RW_Node_Type;
         New_N_A : in     Nodes.Valid_Address_Type)
      is
         L_A : constant Nodes.Address_Type := Nodes.Left_Neighbor(N);
         R_A : constant Nodes.Address_Type := Nodes.Right_Neighbor(N);
      begin
         if Nodes.Is_Valid(L_A) then
            declare
               L : Nodes.RW_Node_Type;
            begin
               Read_Node(Tree, T, Nodes.To_Valid_Address(L_A), L);
               Nodes.Set_Right_Neighbor(L, New_N_A);
               Write_Node(Tree, T, Nodes.To_Valid_Address(L_A), L);
            end;
         end if;
         if Nodes.Is_Valid(R_A) then
            declare
               R : Nodes.RW_Node_Type;
            begin
               Read_Node(Tree, T, Nodes.To_Valid_Address(R_A), R);
               Nodes.Set_Left_Neighbor(R, New_N_A);
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
         M : Nodes.RW_Node_Type;
         N : Nodes.RW_Node_Type;
      begin
         Read_Node(Tree, T, M_A, M);
         Read_Node(Tree, T, N_A, N);
         if not Nodes.Is_Free(M) then
            Prepare_References(Tree, T, M, Old_N_A => M_A, New_N_A => N_A);
         else
            Prepare_Free_References(Tree, T, M, New_N_A => N_A);
         end if;
         if not Nodes.Is_Free(N) then
            Prepare_References(Tree, T, N, Old_N_A => N_A, New_N_A => M_A);
         else
            Prepare_Free_References(Tree, T, N, New_N_A => M_A);
         end if;
      end;
      -- Nodes have to be re-read before being written to their destination.
      declare
         M : Nodes.RW_Node_Type;
         N : Nodes.RW_Node_Type;
      begin
         Read_Node(Tree, T, M_A, M);
         Read_Node(Tree, T, N_A, N);
         Write_Node(Tree, T, N_A, M);
         Write_Node(Tree, T, M_A, N);
      end;
   end Swap_Nodes;


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
      F   : Nodes.RW_Node_Type;
   begin
      Read_Node(Tree, T, F_A, F);
      if Nodes.Is_Valid(Nodes.Right_Neighbor(F)) then
         declare
            N_A : constant Nodes.Valid_Address_Type
                := Nodes.To_Valid_Address(Nodes.Right_Neighbor(F));
            N   : Nodes.RW_Node_Type;
            R_A : Nodes.Address_Type;
         begin
            Read_Node(Tree, T, N_A, N);
            R_A := Nodes.Right_Neighbor(N);
            Nodes.Set_Right_Neighbor(F, R_A);
            Write_Node(Tree, T, F_A, F);
            if Nodes.Is_Valid(R_A) then
               declare
                  R : Nodes.RW_Node_Type;
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
      F   : Nodes.RW_Node_Type;
      N_A : Nodes.Valid_Address_Type renames Address;
      N   : Nodes.RW_Node_Type := Nodes.Free_Node;
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
            R : Nodes.RW_Node_Type;
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
      N     : in     Nodes.RW_Node_Type;
      State : in out State_Type) is
   begin
      if Nodes.Is_Valid(Nodes.Parent(N)) then
         declare
            use type Nodes.Degree_Type;
            P_A     : constant Nodes.Valid_Address_Type
                    := Nodes.Valid_Parent(N);
            P       : Nodes.RW_Node_Type;
            I       : Nodes.Valid_Index_Type;
            Old_Key : Key_Type;
            New_Key : Key_Type;
            New_P   : Nodes.RW_Node_Type;
         begin
            Read_Node(Tree, T, P_A, P);
            I := Nodes.Child_Position(P, N_A);
            Old_Key := Nodes.Key(P, I);
            New_Key := Nodes.Key(N, Nodes.Degree(N));
            New_P := Nodes.Substitution(P, I, New_Key, N_A);
            case Nodes.Validation(New_P) is
               when Nodes.Valid =>
                  Write_Node(Tree, T, P_A, New_P);
                  if I = Nodes.Degree(New_P) and Old_Key /= New_Key then
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
      L     : in     Nodes.RW_Node_Type;
      R_A   : in     Nodes.Valid_Address_Type;
      R     : in     Nodes.RW_Node_Type;
      State : in out State_Type)
   is
      function Combi_Child
        (Left_Node  : Nodes.RW_Node_Type;
         Right_Node : Nodes.RW_Node_Type;
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
      --N : constant Nodes.RW_Node_Type := Nodes.Combination(L, R);
      I     : constant Nodes.Valid_Index_Type
            := Nodes.Combi_Split_Position(L, R);
      L_New : Nodes.RW_Node_Type
            := Nodes.Combi_Copy(L, R, 1, I-1);
      R_New : Nodes.RW_Node_Type
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
                  C   : Nodes.RW_Node_Type;
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
                  C   : Nodes.RW_Node_Type;
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


   procedure Retrieve
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type)
   renames Retrieval.Retrieve;


   procedure Retrieve
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type)
   renames Retrieval.Retrieve;


   procedure Minimum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type)
   renames Retrieval.Minimum;


   procedure Minimum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type)
   renames Retrieval.Minimum;


   procedure Maximum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type)
   renames Retrieval.Maximum;


   procedure Maximum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type)
   renames Retrieval.Maximum;


   procedure Insert
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      State    :    out State_Type)
   renames Insertion.Insert;


   procedure Insert
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      State       :    out State_Type)
   renames Insertion.Insert;


   procedure Delete
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type)
   renames Deletion.Delete;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type)
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


   procedure Finalize_Cursor
     (Tree        : in     Tree_Type;
      Transaction : in     Transaction_Type'Class;
      Cursor      : in out Cursor_Type)
   renames Cursors.Finalize_Cursor;


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
      State       :    out State_Type)
   renames Cursors.Next;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type)
   renames Cursors.Delete;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type)
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
      Height :    out Natural)
   renames Misc.Get_Height;


   procedure Get_Height
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Natural)
   renames Misc.Get_Height;


   procedure Clusterize
     (Tree  : in out Tree_Type;
      State :    out State_Type)
   renames Misc.Clusterize;


   package body Nodes is separate;
   package body Initialization is separate;
   package body Retrieval is separate;
   package body Insertion is separate;
   package body Deletion is separate;
   package body Cursors is separate;
   package body Misc is separate;

end DB.Gen_BTrees;

