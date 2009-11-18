-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

separate (DB.Gen_BTrees)
package body Search is

   procedure Look_Up
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is
      T : RO_Transaction_Type := New_RO_Transaction(Tree);
   begin
      Start_Transaction(Tree, T);
      Look_Up(Tree, T, Key, Value, Position, State);
      Finish_Transaction(Tree, T);
   exception
      when others =>
         Finish_Transaction(Tree, T);
         raise;
   end Look_Up;


   procedure Look_Up
     (Tree     : in out Tree_Type;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type)
   is
      T : RO_Transaction_Type := New_RO_Transaction(Tree);
   begin
      Start_Transaction(Tree, T);
      Look_Up(Tree, T, Position, Value, Key, State);
      Finish_Transaction(Tree, T);
   exception
      when others =>
         Finish_Transaction(Tree, T);
         raise;
   end Look_Up;


   procedure Minimum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is
      T : RO_Transaction_Type := New_RO_Transaction(Tree);
   begin
      Start_Transaction(Tree, T);
      Minimum(Tree, T, Key, Value, Position, State);
      Finish_Transaction(Tree, T);
   exception
      when others =>
         Finish_Transaction(Tree, T);
         raise;
   end Minimum;


   procedure Maximum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is
      T : RO_Transaction_Type := New_RO_Transaction(Tree);
   begin
      Start_Transaction(Tree, T);
      Maximum(Tree, T, Key, Value, Position, State);
      Finish_Transaction(Tree, T);
   exception
      when others =>
         Finish_Transaction(Tree, T);
         raise;
   end Maximum;


   procedure Look_Up
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
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
               Value    := Nodes.Value(N, I);
               Position := Position + Nodes.Count_Sum(N, I);
               State    := Success;
               return;
            end if;
            Position := Position + Nodes.Count_Sum(N, I-1);
            N_A      := Nodes.Child(N, I);
         end;
      end loop;

   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Look_Up;


   procedure Look_Up
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
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
            if Nodes.Is_Leaf(N) then
               Key   := Nodes.Key(N, I);
               Value := Nodes.Value(N, I);
               State := Success;
               return;
            end if;
            Pos := Pos - Nodes.Count_Sum(N, I-1);
            N_A := Nodes.Child(N, I);
         end;
      end loop;

   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Look_Up;


   procedure Minimum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
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
            use type Nodes.Degree_Type;
            N : Nodes.Node_Type;
         begin
            Read_Node(Tree, Transaction, N_A, N);
            if Nodes.Degree(N) = 0 then
               State := Failure;
               pragma Warnings (Off, Position);
               return;
            end if;
            if Nodes.Is_Leaf(N) then
               Key      := Nodes.Key(N, 1);
               Value    := Nodes.Value(N, 1);
               Position := 1;
               State    := Success;
               return;
            end if;
            N_A := Nodes.Child(N, 1);
         end;
      end loop;

   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Minimum;


   procedure Maximum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
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
            use type Nodes.Degree_Type;
            N : Nodes.Node_Type;
         begin
            Read_Node(Tree, Transaction, N_A, N);
            if Nodes.Degree(N) = 0 then
               State := Failure;
               pragma Warnings (Off, Position);
               return;
            end if;
            if Nodes.Is_Root(N) then
               Position := Nodes.Count_Sum(N);
            end if;
            if Nodes.Is_Leaf(N) then
               Key      := Nodes.Key(N, Nodes.Degree(N));
               Value    := Nodes.Value(N, Nodes.Degree(N));
               State    := Success;
               return;
            end if;
            N_A := Nodes.Child(N, Nodes.Degree(N));
         end;
      end loop;

   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Maximum;

end Search;

