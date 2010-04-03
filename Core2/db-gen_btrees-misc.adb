-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.Gen_BTrees)
package body Misc is

   procedure Count
     (Tree  : in out Tree_Type;
      Count :    out Count_Type)
   is
      Transaction : RO_Transaction_Type := New_RO_Transaction(Tree);
   begin
      Start_Transaction(Tree, Transaction);
      Misc.Count(Tree, Transaction, Count);
      Finish_Transaction(Tree, Transaction);
   exception
      when others =>
         Finish_Transaction(Tree, Transaction);
         raise;
   end Count;


   procedure Count
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Count       :    out Count_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      pragma Assert (Transaction.Started);

      N_A : Nodes.Valid_Address_Type;
   begin
      N_A := Transaction.Current_Root_Address;
      loop
         declare
            N : Nodes.RO_Node_Type;
         begin
            Read_Node(Tree, Transaction, N_A, N);
            exit when Nodes.Is_Leaf(N);
            N_A := Nodes.Child(N, 1);
         end;
      end loop;
      Count := 0;
      loop
         declare
            N : Nodes.RO_Node_Type;
         begin
            Read_Node(Tree, Transaction, N_A, N);
            Count := Count + Count_Type(Nodes.Degree(N));
            exit when not Nodes.Is_Valid(Nodes.Right_Neighbor(N));
            N_A := Nodes.Valid_Right_Neighbor(N);
         end;
      end loop;
   end Count;


   procedure Get_Height
     (Tree   : in out Tree_Type;
      Height :    out Natural)
   is
      Transaction : RO_Transaction_Type := New_RO_Transaction(Tree);
   begin
      Start_Transaction(Tree, Transaction);
      Get_Height(Tree, Transaction, Height);
      Finish_Transaction(Tree, Transaction);
   exception
      when others =>
         Finish_Transaction(Tree, Transaction);
         raise;
   end Get_Height;


   procedure Get_Height
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Natural)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      pragma Assert (Transaction.Started);

      N_A : Nodes.Valid_Address_Type := Transaction.Current_Root_Address;
   begin
      Height := 0;
      loop
         declare
            use type Nodes.Degree_Type;
            N : Nodes.RO_Node_Type;
         begin
            Read_Node(Tree, Transaction, N_A, N);
            if Height = 0 and Nodes.Degree(N) > 0 then
               Height := 1;
            end if;
            exit when Nodes.Is_Leaf(N);
            N_A := Nodes.Child(N, 1);
            Height := Height + 1;
         end;
      end loop;
   end Get_Height;


   procedure Clusterize
     (Tree  : in out Tree_Type;
      State :    out State_Type)
   is
      procedure Traverse_Breadth_First
        (Tree   : in out Tree_Type;
         T      : in out RO_Transaction_Type'Class;
         State  :    out State_Type)
      is
         procedure Find_Outermost_Left_In_Level
           (Tree   : in out Tree_Type;
            T      : in out RO_Transaction_Type'Class;
            Level  : in     Natural;
            N_A    :    out Nodes.Valid_Address_Type;
            State  :    out State_Type)
         is
            Current_Level : Natural := 1;
         begin
            N_A := Root_Address;
            loop
               declare
                  use type Nodes.Degree_Type;
                  N : Nodes.RO_Node_Type;
               begin
                  Read_Node(Tree, T, N_A, N);
                  if not Nodes.Is_Valid(N) or else
                     (Nodes.Degree(N) = 0 or Nodes.Is_Leaf(N)) then
                     State := Failure;
                     return;
                  end if;
                  if Current_Level + 1 = Level then
                     N_A   := Nodes.Child(N, 1);
                     State := Success;
                     return;
                  end if;
                  N_A           := Nodes.Child(N, 1);
                  Current_Level := Current_Level + 1;
               end;
            end loop;
         end Find_Outermost_Left_In_Level;

         procedure Increment
           (N_A : in out Nodes.Valid_Address_Type) is
         begin
            N_A := Nodes.Valid_Address_Type(Block_IO.Succ
                       (Block_IO.Valid_Address_Type(N_A)));
         end Increment;

         Level : Natural := 1;
         N_A   : Nodes.Valid_Address_Type;
         NN_A  : Nodes.Valid_Address_Type;
      begin
         declare
            use type Nodes.Degree_Type;
            N : Nodes.RO_Node_Type;
         begin
            Read_Node(Tree, T, Root_Address, N);
            if Nodes.Degree(N) = 0 or Nodes.Is_Leaf(N) then
               State := Success;
               return;
            end if;
         end;

         Level := Level + 1;
         Find_Outermost_Left_In_Level(Tree, T, Level, N_A, State);
         NN_A := Free_Address;
         Increment(NN_A);
         if State /= Success then
            return;
         end if;

         loop
            declare
               procedure Swap
                 (Tree   : in out Tree_Type;
                  T      : in out RO_Transaction_Type'Class;
                  N_A    : in     Nodes.Valid_Address_Type;
                  NN_A   : in     Nodes.Valid_Address_Type)
               is
                  Sub_T : Sub_RW_Transaction_Type
                        := New_Sub_RW_Transaction(Tree, T);
               begin
                  Start_Transaction(Tree, Sub_T);
                  Swap_Nodes(Tree, Sub_T, N_A, NN_A);
                  Commit_Transaction(Tree, Sub_T);
               exception
                  when others =>
                     Abort_Transaction(Tree, Sub_T);
                     raise;
               end Swap;

               use type Nodes.Degree_Type;
               N : Nodes.RO_Node_Type;
            begin
               Read_Node(Tree, T, N_A, N);
               Swap(Tree, T, N_A, NN_A);
               if Nodes.Is_Valid(Nodes.Right_Neighbor(N)) then
                  declare
                     NN : Nodes.RO_Node_Type;
                  begin
                     Read_Node(Tree, T, NN_A, NN);
                     N_A := Nodes.To_Valid_Address(Nodes.Right_Neighbor(NN));
                  end;
                  Increment(NN_A);
               elsif Nodes.Is_Inner(N) then
                  Level := Level + 1;
                  Find_Outermost_Left_In_Level(Tree, T, Level, N_A, State);
                  Increment(NN_A);
                  if State /= Success then
                     return;
                  end if;
               else
                  State := Success;
                  return;
               end if;
            end;
         end loop;
      end Traverse_Breadth_First;

      pragma Assert (Tree.Initialized);
      T : RO_Transaction_Type := New_RO_Transaction(Tree);
   begin
      Traverse_Breadth_First(Tree, T, State);
      Finish_Transaction(Tree, T);
   exception
      when others =>
         Finish_Transaction(Tree, T);
         raise;
   end Clusterize;

end Misc;

