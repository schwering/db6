-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.Gen_BTrees)
procedure Delete
  (Tree     : in out Tree_Type;
   Key      : in     Key_Type;
   Value    :    out Value_Type;
   State    :    out State_Type)
is
   procedure Find_Leaf_Address
     (N_A : out Nodes.Valid_Address_Type) is
   begin
      N_A := Root_Address;
      loop
         declare
            N : Nodes.RO_Node_Type;
         begin
            Read_Node(Tree, N_A, N);
            exit when Nodes.Is_Leaf(N);
            N_A := Scan_Node(N, Key);
         end;
      end loop;
   end Find_Leaf_Address;

   function Exit_Condition
     (N_A : Nodes.Valid_Address_Type;
      N   : Nodes.Node_Type)
      return Boolean
   is
      pragma Unreferenced (N_A);
      use type Nodes.Degree_Type;
      High_Key     : Key_Type;
      Has_High_Key : Boolean;
   begin
      if not Nodes.Is_Valid(Nodes.Link(N)) then
         return True;
      end if;
      Nodes.Get_High_Key(N, High_Key, Has_High_Key);
      if not Has_High_Key then
         return False;
      end if;
      if Nodes.Degree(N) = 0 then
         return Key < High_Key;
      else
         return Key <= High_Key;
      end if;
   end Exit_Condition;

   procedure Delete
     (N_A   : in Nodes.Valid_Address_Type;
      N_Old : in Nodes.Node_Type)
   is
      I : constant Nodes.Index_Type := Nodes.Key_Position(N_Old, Key);
   begin
      if Nodes.Is_Valid(I) and then Key = Nodes.Key(N_Old, I) then
         declare
            N : constant Nodes.RW_Node_Type := Nodes.Deletion(N_Old, I);
         begin
            Write_Node(Tree, N_A, N);
            Value := Nodes.Value(N_Old, I);
            State := Success;
         end;
      else
         State := Failure;
      end if;
   end Delete;

   N_A : Nodes.Valid_Address_Type;
   N   : Nodes.RW_Node_Type;
begin
   Find_Leaf_Address(N_A);
   Move_Right(Tree, Exit_Condition'Access, N_A, N);
   declare
   begin
      Delete(N_A, N);
   exception
      when others =>
         Unlock(Tree, N_A);
         raise;
   end;
   Unlock(Tree, N_A);
end Delete;

