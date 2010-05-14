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

   procedure Pop_Inner
     (C_A : in  Nodes.Valid_Address_Type;
      N_A : out Nodes.Valid_Address_Type;
      N   : out Nodes.RW_Node_Type)
   is
      function Exit_Cond (N : Nodes.Node_Type) return Boolean is
      begin
         return Nodes.Is_Valid(Nodes.Child_Position(N, C_A));
      end Exit_Cond;

      Level : Nodes.Level_Type;
   begin
      declare
      begin
         Stacks.Pop(Stack, N_A, Level);
         Stacks.Move_Right(Tree, Stack, Level, Exit_Cond'Access, N_A, N);
      exception
         when others =>
            Unlock(Tree, C_A);
            raise;
      end;
      Unlock(Tree, C_A);
   end Pop_Inner;


   procedure Write_And_Ascend
     (N_A   : in Nodes.Valid_Address_Type;
      N_Old : in Nodes.RW_Node_Type;
      N     : in Nodes.RW_Node_Type);


   -- Handles the case that the high-key of a node C changed. Then C_A is the
   -- address of C and C_Key is the high-key of C.
   procedure Update_High_Key
     (C_Key : in Key_Type;
      C_A   : in Nodes.Valid_Address_Type) is
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
            I := Nodes.Child_Position(N_Old, C_A);
            N := Nodes.Substitution(N_Old, I, C_Key, C_A);
            Write_And_Ascend(N_A, N_Old, N);
         end;
      end if;
   end Update_High_Key;


   -- Writes back the node(s) visited of the current level.
   -- The address N_A must be locked when this procedure is called.
   -- N_A is unlocked by this procedure.
   procedure Write_And_Ascend
     (N_A   : in Nodes.Valid_Address_Type;
      N_Old : in Nodes.RW_Node_Type;
      N     : in Nodes.RW_Node_Type)
   is
      use type Nodes.Degree_Type;
   begin
      declare
      begin
         Write_Node(Tree, N_A, N);
      exception
         when others =>
            Unlock(Tree, N_A);
            raise;
      end;
      if not Nodes.Has_High_Key(N_Old) or else
         Nodes.High_Key(N_Old) /= Nodes.High_Key(N) then
         Update_High_Key(Nodes.High_Key(N), N_A);
      else
         Unlock(Tree, N_A);
         State := Success;
      end if;
   end Write_And_Ascend;

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
            Write_And_Ascend(N_A, N_Old, N);
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

