separate (DB.Gen_BTrees)
package body Stacks is

   procedure Initialize
     (Stack : out Stack_Type;
      Key   : in  Key_Type) is
   begin
      Stack := (S => Stacks.New_Stack, Key => Key);
   end Initialize;

   procedure Finalize
     (Stack : in out Stack_Type) is
   begin
      Stacks.Finalize(Stack.S);
   end Finalize;

   procedure Clear
     (Stack : in out Stack_Type) is
   begin
      Stacks.Clear(Stack.S);
   end Clear;

   function Is_Empty
     (Stack : Stack_Type)
      return Boolean is
   begin
      return Stacks.Is_Empty(Stack.S);
   end Is_Empty;

   procedure Push
     (Stack : in out Stack_Type;
      N_A   : in Nodes.Valid_Address_Type;
      Level : in Nodes.Level_Type) is
   begin
      Stacks.Push(Stack.S, Item_Type'(N_A, Level));
   end Push;

   procedure Pop
     (Stack : in out Stack_Type;
      N_A   :    out Nodes.Valid_Address_Type)
   is
      Level : Nodes.Level_Type;
   begin
      Pop(Stack, N_A, Level);
   end Pop;

   procedure Pop
     (Stack : in out Stack_Type;
      N_A   :    out Nodes.Valid_Address_Type;
      Level :    out Nodes.Level_Type)
   is
      Item : Item_Type;
   begin
      Stacks.Pop(Stack.S, Item);
      N_A := Item.Address;
      Level := Item.Level;
   end Pop;


   procedure Build_Stack
     (Tree  : in out Tree_Type;
      Stack : in out Stack_Type;
      Level : in     Nodes.Level_Type)
   is
      use type Nodes.Level_Type;
      N_A : Nodes.Valid_Address_Type;
      N   : Nodes.RO_Node_Type;
   begin
      Clear(Stack);

      N_A := Root_Address;
      Read_Node(Tree, N_A, N);
      if Nodes.Is_Valid(Nodes.Link(N)) then
         -- Lock root to enforce that it is the first node in the stack.
         Lock(Tree, N_A);
         declare
         begin
            Read_Node(Tree, N_A, N);
            pragma Assert (not Nodes.Is_Valid(Nodes.Link(N)));
         exception
            when others =>
               Unlock(Tree, N_A);
               raise;
         end;
         Unlock(Tree, N_A);
      end if;

      loop
         if Nodes.Level(N) = Level then
            Push(Stack, N_A, Nodes.Level(N));
            exit;
         end if;
         pragma Assert (Nodes.Level(N) > Level);
         declare
            use type Nodes.Address_Type;
            NN_A : constant Nodes.Valid_Address_Type := Scan_Node(N, Stack.Key);
         begin
            if Nodes.To_Address(NN_A) /= Nodes.Link(N) then
               Push(Stack, N_A, Nodes.Level(N));
            end if;
            N_A := NN_A;
         end;
         Read_Node(Tree, N_A, N);
      end loop;
   end Build_Stack;


   procedure Move_Right
     (Tree      : in out          Tree_Type;
      Stack     : in out          Stack_Type;
      Level     : in              Nodes.Level_Type;
      Exit_Cond : not null access function (N : Nodes.Node_Type) return Boolean;
      N_A       : in out          Nodes.Valid_Address_Type;
      N         :    out          Nodes.Node_Type)
   is
      use type Nodes.Level_Type;
      use type Nodes.Valid_Address_Type;

      function Precise_Exit_Cond (N : Nodes.Node_Type) return Boolean is
      begin
         if Nodes.Level(N) /= Level then
            pragma Assert (Is_Empty(Stack));
            return True;
         end if;
         return Exit_Cond(N);
      end Precise_Exit_Cond;
   begin
      Move_Right(Tree, Precise_Exit_Cond'Access, N_A, N);
      if Nodes.Level(N) /= Level then
         pragma Assert (N_A = Root_Address);
         Unlock(Tree, N_A);
         Build_Stack(Tree, Stack, Level);
         Pop(Stack, N_A);
         pragma Assert (N_A /= Root_Address);
         Move_Right(Tree, Precise_Exit_Cond'Access, N_A, N);
         pragma Assert (Nodes.Level(N) = Level);
      end if;
   end Move_Right;


   procedure Write_And_Ascend
     (Tree  : in out Tree_Type;
      Stack : in out Stack_Type;
      N_A   : in     Nodes.Valid_Address_Type;
      N_Old : in     Nodes.RW_Node_Type;
      N     : in     Nodes.RW_Node_Type)
   is
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
            Pop(Stack, N_A, Level);
            Move_Right(Tree, Stack, Level, Exit_Cond'Access, N_A, N);
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
         if Is_Empty(Stack) then
            Unlock(Tree, C_A);
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


      -- Handles the split of a node into nodes L and R, where L is written back
      -- to the position of the original node, L_A, and R is written to a new
      -- address, R_A.
      -- If there is no parent into which the subtree induced by R can be
      -- inserted, i.e. the stack is empty, then we've arrived at the root and
      -- create a new one.
      -- Otherwise, in the parent N of L, the key of the pointer to L is set to
      -- the possibly changed high-key of L and the pointer to R and its
      -- high-key are inserted.
      procedure Insert_Key_And_Update_High_Key
        (L_Key : in Key_Type;
         L_A   : in Nodes.Valid_Address_Type;
         R_Key : in Key_Type;
         R_A   : in Nodes.Valid_Address_Type)
      is
         use type Nodes.Valid_Address_Type;
      begin
         if Is_Empty(Stack) then
            -- Create a new root that points to L and R.
            pragma Assert (L_A = Root_Address);
            declare
               use type Nodes.Level_Type;
               N_A     : constant Nodes.Valid_Address_Type := L_A;
               N       : Nodes.RW_Node_Type;
               L       : Nodes.RW_Node_Type;
               L_A_New : Nodes.Valid_Address_Type;
            begin
               Read_Node(Tree, L_A, L);
               pragma Assert (Nodes.Valid_Link(L) = R_A);
               Write_New_Node(Tree, L_A_New, L);
               N := Nodes.Root_Node(Nodes.Level(L) + 1);
               N := Nodes.Insertion(N, 1, L_Key, L_A_New);
               N := Nodes.Insertion(N, 2, R_Key, R_A);
               Nodes.Set_Link(N, Invalid_Address);
               Write_Node(Tree, N_A, N);
            exception
               when others =>
                  Unlock(Tree, L_A);
                  Unlock(Tree, R_A);
                  raise;
            end;
            Unlock(Tree, L_A);
            Unlock(Tree, R_A);
         else
            -- Update high key of L and insert high key of R.
            declare
               use type Nodes.Index_Type;
               N_A   : Nodes.Valid_Address_Type;
               N_Old : Nodes.RW_Node_Type;
               I     : Nodes.Index_Type;
               N     : Nodes.RW_Node_Type;
            begin
               declare
               begin
                  Pop_Inner(L_A, N_A, N_Old);
               exception
                  when others =>
                     Unlock(Tree, R_A);
                     raise;
               end;
               Unlock(Tree, R_A);
               I := Nodes.Child_Position(N_Old, L_A);
               N := Nodes.Substitution(N_Old, I, L_Key, L_A);
               I := I + 1;
               N := Nodes.Insertion(N, I, R_Key, R_A);
               Write_And_Ascend(N_A, N_Old, N);
            end;
         end if;
      end Insert_Key_And_Update_High_Key;


      procedure Write_And_Ascend
        (N_A   : in Nodes.Valid_Address_Type;
         N_Old : in Nodes.RW_Node_Type;
         N     : in Nodes.RW_Node_Type)
      is
         use type Nodes.Degree_Type;
      begin
         if Nodes.Is_Safe(N, Is_Root => Is_Empty(Stack)) then
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
            end if;
         else
            declare
               I   : constant Nodes.Valid_Index_Type := Nodes.Split_Position(N);
               L   : Nodes.RW_Node_Type := Nodes.Copy(N, 1, I - 1);
               R   : Nodes.RW_Node_Type := Nodes.Copy(N, I, Nodes.Degree(N));
               L_A : Nodes.Valid_Address_Type renames N_A;
               R_A : Nodes.Valid_Address_Type;
            begin
               Nodes.Set_Link(R, Nodes.Link(N));
               Write_New_Node(Tree, R_A, R);
               Lock(Tree, R_A);
               Nodes.Set_Link(L, R_A);
               Write_Node(Tree, L_A, L);
               Insert_Key_And_Update_High_Key(Nodes.High_Key(L), L_A,
                                              Nodes.High_Key(R), R_A);
            end;
         end if;
      end Write_And_Ascend;

   begin
      Write_And_Ascend(N_A, N_Old, N);
   end Write_And_Ascend;

end Stacks;

