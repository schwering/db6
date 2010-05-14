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

end Stacks;

