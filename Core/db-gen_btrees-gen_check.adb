-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with DB.IO.Blocks;

procedure DB.Gen_BTrees.Gen_Check
  (Tree : in out Tree_Type)
is
   Transaction : RO_Transaction_Type := New_RO_Transaction(Tree);

   procedure Read_Node
     (Tree        : in out Tree_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           :    out Nodes.Node_Type) is
   begin
      Read_Node(Tree, Transaction, N_A, N);
   end Read_Node;

   procedure Read_Node
     (Tree        : in out Tree_Type;
      N_A         : in     Nodes.Address_Type;
      N           :    out Nodes.Node_Type) is
   begin
      Read_Node(Tree, Transaction, Nodes.To_Valid_Address(N_A), N);
   end Read_Node;


   function Image (N_A : Nodes.Valid_Address_Type) return String is
   begin
      return Block_IO.Image(Block_IO.Valid_Address_Type(N_A));
   end Image;

   function Image (N_A : Nodes.Address_Type) return String is
   begin
      if Nodes.Is_Valid(N_A) then
         return Image(Nodes.To_Valid_Address(N_A));
      else
         return "invalid";
      end if;
   end Image;

   procedure Check_Tree
     (Tree : in out Tree_Type)
   is
      Height     : Natural := 0;
      Inv_Left   : Natural := 0;
      Inv_Right  : Natural := 0;
      Inv_Parent : Natural := 0;

      function Node_To_String (N : Nodes.Node_Type) return String
      is
         procedure App (L : in out Natural; S : in String) is
         begin
            L := L + S'Length + 1;
         end App;

         Offset : Natural := 1;
         procedure App (S : in out String; T : in String) is
         begin
            S(Offset .. Offset + T'Length - 1) := T;
            for I in Offset .. Offset + T'Length - 1 loop
               if S(I) not in 'A'..'Z' and
                  S(I) not in 'a'..'z' and
                  S(I) not in '0'..'9' and
                  S(I) /= '=' and
                  S(I) /= '(' and
                  S(I) /= ')' and
                  S(I) /= ' ' then
                  S(I) := '_';
               end if;
            end loop;
            Offset := Offset + T'Length;
            S(Offset) := ASCII.LF;
            Offset := Offset + 1;
         end App;

         function To_String (A : Nodes.Address_Type) return String is
         begin
            return Address_To_String(Block_IO.Address_Type(A));
         end To_String;

         function To_String (A : Nodes.Valid_Address_Type) return String is
         begin
            return Address_To_String(Block_IO.Address_Type
                     (Nodes.To_Address(A)));
         end To_String;

         L : Natural := 0;
      begin
         App(L, "Is_Free = "& Boolean'Image(Nodes.Is_Free(N)));
         if not Nodes.Is_Free(N) then
            App(L, "Degree ="& Nodes.Degree_Type'Image(Nodes.Degree(N)));
            App(L, "Is_Leaf = "& Boolean'Image(Nodes.Is_Leaf(N)));
            App(L, "Parent = "& To_String(Nodes.Parent(N)));
            App(L, "Left = "& To_String(Nodes.Left_Neighbor(N)));
            App(L, "Right = "& To_String(Nodes.Right_Neighbor(N)));
            for I in 1 .. Nodes.Degree(N) loop
               App(L, "Child"& Nodes.Degree_Type'Image(I) &" ");
               App(L, "   "& Key_To_String(Nodes.Key(N, I)));
               if Nodes.Is_Inner(N) then
                  App(L, "   "& To_String(Nodes.Child(N, I)));
               else
                  App(L, "   "& Value_To_String(Nodes.Value(N, I)));
               end if;
            end loop;
         end if;

         declare
            S : String(1 .. L);
         begin
            App(S, "Is_Free = "& Boolean'Image(Nodes.Is_Free(N)));
            if not Nodes.Is_Free(N) then
               App(S, "Degree ="& Nodes.Degree_Type'Image(Nodes.Degree(N)));
               App(S, "Is_Leaf = "& Boolean'Image(Nodes.Is_Leaf(N)));
               App(S, "Parent = "& To_String(Nodes.Parent(N)));
               App(S, "Left = "& To_String(Nodes.Left_Neighbor(N)));
               App(S, "Right = "& To_String(Nodes.Right_Neighbor(N)));
               for I in 1 .. Nodes.Degree(N) loop
                  App(S, "Child"& Nodes.Degree_Type'Image(I) &" ");
                  App(S, "   "& Key_To_String(Nodes.Key(N, I)));
                  if Nodes.Is_Inner(N) then
                     App(S, "   "& To_String(Nodes.Child(N, I)));
                  else
                     App(S, "   "& Value_To_String(Nodes.Value(N, I)));
                  end if;
               end loop;
            end if;
            Ada.Text_IO.Put_Line(S);
            Ada.Text_IO.New_Line;
            Ada.Text_IO.New_Line;
            Ada.Text_IO.New_Line;
            return S;
         end;
      end Node_To_String;

      function Message (Text : String; N : Nodes.Node_Type) return String is
      begin
         Ada.Text_IO.Put_Line(Text);
         return Text & " "& Node_To_String(N);
      end Message;

      function Message (Text : String; M, N : Nodes.Node_Type) return String is
      begin
         return Text & " "& Node_To_String(M) & Node_To_String(N);
      end Message;

      procedure Check_Subtree
        (Address : in Nodes.Valid_Address_Type;
         Level   : in Natural);

      Counter  : Positive := 1;
      Interval : constant Positive := 1024;

      procedure Check_Subtree
        (Address : in Nodes.Valid_Address_Type;
         Level   : in Natural)
      is
         N : Nodes.RO_Node_Type;
      begin
         if Counter mod Interval = 0 then
            null;
            Ada.Text_IO.Put_Line("Check:"& Positive'Image(Counter*4/1024));
         end if;
         Counter := Counter + 1;

         if Level > Height then
            Height := Level;
         end if;

         Read_Node(Tree, Address, N);
         declare
            use type Nodes.Address_Type;
            use type Nodes.Index_Type;
         begin
            if not Nodes.Is_Valid(N) then
               Raise_Exception(Tree_Error'Identity,
                               Message("Node is not valid", N));
               return;
            end if;

            -- Check unfree
            if Nodes.Is_Free(N) then
               Raise_Exception(Tree_Error'Identity,
                               Message("Node is free", N));
               return;
            end if;

            -- Check parent
            if Nodes.Is_Valid(Nodes.Parent(N)) then
               declare
                  use type Nodes.Valid_Address_Type;
                  P : Nodes.RO_Node_Type;
                  Found : Boolean := False;
               begin
                  Read_Node(Tree, Nodes.Parent(N), P);
                  for I in 1 .. Nodes.Degree(P) loop
                     if Nodes.Child(P, I) = Address then
                        Found := True;
                        exit;
                     end if;
                  end loop;
                  if not Found then
                     Raise_Exception(Tree_Error'Identity,
                                     Message("Parent doesn't point to child",
                                             P, N));
                  end if;

                  if not (Nodes.Key(N, Nodes.Degree(N))
                     <= Nodes.Key(P, Nodes.Child_Position(P, Address))) then
                     Raise_Exception(Tree_Error'Identity,
                                     Message("Parent key not greater", P, N));
                  end if;
               end;
            else
               Inv_Parent := Inv_Parent + 1;
            end if;

            -- Check key order
            for I in 2 .. Nodes.Degree(N) loop
               if not (Nodes.Key(N, I-1) <= Nodes.Key(N, I)) then
Ada.Text_IO.Put_Line("ADDRESS ="& Address_To_String(Block_IO.Address_Type(Nodes.To_Address(Address))));
Ada.Text_IO.Put_Line("CONFLICTING CHILD "&
   Nodes.Degree_Type'Image(I-1) &
   Nodes.Degree_Type'Image(I));
declare

   function R(A : Nodes.Valid_Address_Type) return Nodes.Node_Type
   is
      N : Nodes.RO_Node_Type;
   begin
      Read_Node(Tree, A, N);
      return N;
   end R;

   function R(A : Nodes.Address_Type) return Nodes.Node_Type is
   begin
      return R(Nodes.To_Valid_Address(A));
   end R;
   pragma Unreferenced (R);

   --S : String := Message("PARENT PARENT PARENT", R(Nodes.Parent(N)));
   --U : String := Message("LEFT LEFT LEFT", R(Nodes.Left_Neighbor(N)));
   --T : String := Message("NODE NODE NODE", N);
   --V : String := Message("RIGHT RIGHT RIGHT", R(Nodes.Right_Neighbor(N)));
begin
   for I in 1 .. Nodes.Degree(N) loop
      declare
         --X : String := Message("CHILD "& Nodes.Degree_Type'Image(I),
                               --R(Nodes.Child(N, I)));
      begin
         null;
      end;
   end loop;
end;
                  Raise_Exception(Tree_Error'Identity,
                                  Message("Key order im Arsch:"&
                                  Nodes.Index_Type'Image(I-1) &","&
                                  Nodes.Index_Type'Image(I),
                                  N));
               end if;
            end loop;

            -- Check left neighbor
            if Nodes.Is_Valid(Nodes.Left_Neighbor(N)) then
               declare
                  use type Nodes.Valid_Address_Type;
                  L : Nodes.RO_Node_Type;
               begin
                  Read_Node(Tree, Nodes.Left_Neighbor(N), L);
                  if Nodes.Valid_Right_Neighbor(L) /= Address then
                     Raise_Exception(Tree_Error'Identity,
                             Message("Left neighbor doesn't point back", L, N));
                     return;
                  end if;
                  if Nodes.Is_Inner(L) /= Nodes.Is_Inner(N) then
                     Raise_Exception(Tree_Error'Identity,
                                     Message("Left branch has different height",
                                             L, N));
                     return;
                  end if;
                  if not (Nodes.Key(L, Nodes.Degree(L)) <= Nodes.Key(N, 1)) then
declare

   function R(A : Nodes.Valid_Address_Type) return Nodes.Node_Type
   is
      N : Nodes.RO_Node_Type;
   begin
      Read_Node(Tree, A, N);
      return N;
   end R;

   function R(A : Nodes.Address_Type) return Nodes.Node_Type is
   begin
      return R(Nodes.To_Valid_Address(A));
   end R;
   pragma Unreferenced (R);

   S : String := Message("PARENT PARENT PARENT", R(Nodes.Parent(N)));
   U : String := Message("LEFT LEFT LEFT", R(Nodes.Left_Neighbor(N)));
   T : String := Message("NODE NODE NODE", N);
   V : String := Message("RIGHT RIGHT RIGHT", R(Nodes.Right_Neighbor(N)));
begin
   for I in 1 .. Nodes.Degree(N) loop
      declare
         X : String := Message("CHILD "& Nodes.Degree_Type'Image(I),
                               R(Nodes.Child(N, I)));
      begin
         null;
      end;
   end loop;
end;
                     Raise_Exception(Tree_Error'Identity,
                                     Message("Left key not smaller", L, N));
                  end if;
               end;
            else
               Inv_Left := Inv_Left + 1;
            end if;

            -- Check right neighbor
            if Nodes.Is_Valid(Nodes.Right_Neighbor(N)) then
               declare
                  use type Nodes.Valid_Address_Type;
                  R : Nodes.RO_Node_Type;
               begin
                  Read_Node(Tree, Nodes.Right_Neighbor(N), R);
                  if Nodes.Valid_Left_Neighbor(R) /= Address then
                     Raise_Exception(Tree_Error'Identity,
                                  Message("Right neighbor doesn't point back",
                                          N, R));
                     return;
                  end if;
                  if Nodes.Is_Inner(N) /= Nodes.Is_Inner(R) then
                     Raise_Exception(Tree_Error'Identity,
                                  Message("Right branch has different height",
                                          N, R));
                     return;
                  end if;
                  if not (Nodes.Key(N, Nodes.Degree(N)) <= Nodes.Key(R, 1)) then
                     Raise_Exception(Tree_Error'Identity,
                                     Message("Right key not greater",
                                             N, R));
                  end if;
               end;
            else
               Inv_Right := Inv_Right + 1;
            end if;

            -- Depth first search
            if Nodes.Is_Inner(N) then
               for I in 1 .. Nodes.Degree(N) loop
                  Check_Subtree(Nodes.Child(N, I), Level + 1);
               end loop;
            end if;
         end;
      end Check_Subtree;

      Tree_Height : Natural;
   begin -- Tree_Check
      Check_Subtree(Root_Address, 1);
      Get_Height(Tree, Tree_Height);
      if Tree_Height = 0 then
         Tree_Height := 1;
      end if;
      if Height /= Tree_Height then
         Raise_Exception(Tree_Error'Identity,
                         "Height ="& Natural'Image(Height) &", but "&
                         "Tree.Height ="& Natural'Image(Tree_Height));
      elsif Inv_Left /= Tree_Height then
         Raise_Exception(Tree_Error'Identity,
                         "Inv_Left ="& Natural'Image(Inv_Left) &", but "&
                         "Tree.Height ="& Natural'Image(Tree_Height));
      elsif Inv_Right /= Tree_Height then
         Raise_Exception(Tree_Error'Identity,
                         "Inv_Left ="& Natural'Image(Inv_Right) &", but "&
                         "Tree.Height ="& Natural'Image(Tree_Height));
      elsif (Tree_Height = 0 and Inv_Parent /= 0) or
         (Tree_Height > 0 and Inv_Parent /= 1) then
         Raise_Exception(Tree_Error'Identity,
                        "Inv_Parent ="& Natural'Image(Inv_Parent) &", but "&
                        "Tree.Height ="& Natural'Image(Tree_Height));
      end if;
   end Check_Tree;


   procedure Check_Free_Nodes
     (Tree  : in out Tree_Type)
   is
      Address  : Nodes.Address_Type;
      Block    : IO.Blocks.Block_Type;
      Count1   : Natural := 0;
      Count2   : Natural := 0;
   begin
      Address := Nodes.To_Address(Free_Address);
      while Nodes.Is_Valid(Address) loop
         declare
            N : Nodes.RO_Node_Type;
         begin
            Read_Node(Tree, Address, N);
            if not Nodes.Is_Free(N) then
               Raise_Exception(Tree_Error'Identity,
                               "Node is not free but in list "& Image(Address));
            end if;
            Count1 := Count1 + 1;
            Address := Nodes.Right_Neighbor(N);
         end;
      end loop;

      Address := Nodes.To_Address(Nodes.Valid_Address_Type(Block_IO.First));
      loop
         declare
         begin
            declare
               N : Nodes.RO_Node_Type;
            begin
               Read_Node(Tree, Address, N);
               if Nodes.Is_Free(N) then
                  Count2 := Count2 + 1;
               end if;
            end;
            Address := Nodes.To_Address(Nodes.Valid_Address_Type(Block_IO.Succ
                        (Block_IO.Valid_Address_Type
                           (Nodes.To_Valid_Address(Address)))));
         exception
            when IO_Error =>
               exit;
         end;
      end loop;

      if Count1 /= Count2 then
         Raise_Exception(Tree_Error'Identity,
                         "Found"&Natural'Image(Count1)&" free nodes in list, "&
                         "but"&Natural'Image(Count2)&" free nodes in file");
      end if;
   end Check_Free_Nodes;

   Ticket : Block_IO.Ticket_Type;
begin -- Gen_Check
   Start_Transaction(Tree, Transaction);
   Check_Tree(Tree);
   Check_Free_Nodes(Tree);
   Finish_Transaction(Tree, Transaction);
exception
   when others =>
      Finish_Transaction(Tree, Transaction);
      raise;
end DB.Gen_BTrees.Gen_Check;

