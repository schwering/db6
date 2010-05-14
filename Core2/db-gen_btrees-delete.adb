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
   pragma Assert (Tree.Initialized);

   Stack : Stacks.Stack_Type;
   N_A   : Nodes.Valid_Address_Type;
   N_Old : Nodes.RW_Node_Type;
begin
   Stacks.Initialize(Stack, Key);

   declare
      function Exit_Cond
        (N : Nodes.Node_Type)
         return Boolean
      is
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
      end Exit_Cond;
   begin
      Stacks.Build_Stack(Tree, Stack, Nodes.Leaf_Level);
      Stacks.Pop(Stack, N_A);
      Stacks.Move_Right(Tree, Stack, Nodes.Leaf_Level, Exit_Cond'Access,
                        N_A, N_Old);
   end;

   declare
      I : Nodes.Index_Type;
   begin
      I := Nodes.Key_Position(N_Old, Key);
      if Nodes.Is_Valid(I) and then Key = Nodes.Key(N_Old, I) then
         declare
            N : constant Nodes.RW_Node_Type := Nodes.Deletion(N_Old, I);
         begin
            Stacks.Write_And_Ascend(Tree, Stack, N_A, N_Old, N);
            Value := Nodes.Value(N_Old, I);
            State := Success;
         end;
      else
         Stacks.Finalize(Stack);
         State := Failure;
         return;
      end if;
   end;

   Stacks.Finalize(Stack);

exception
   when others =>
      Stacks.Finalize(Stack);
      pragma Warnings (Off);
      State := Error;
      pragma Warnings (On);
      raise;
end Delete;

