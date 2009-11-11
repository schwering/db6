separate (DB.Gen_BTrees)
package body Cursors is

   procedure Lock_Mutex
     (Cursor : in out Cursor_Type) is
   begin
      if Cursor.Thread_Safe then
         Locks.Mutexes.Lock(Cursor.Mutex);
      end if;
   end Lock_Mutex;


   procedure Unlock_Mutex
     (Cursor : in out Cursor_Type) is
   begin
      if Cursor.Thread_Safe then
         Locks.Mutexes.Unlock(Cursor.Mutex);
      end if;
   end Unlock_Mutex;


   function Positive_Infinity_Bound
      return Bound_Type is
   begin
      return Bound_Type'(Kind => Abstract_Bound, Location => Positive_Infinity);
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound
      return Bound_Type is
   begin
      return Bound_Type'(Kind => Abstract_Bound, Location => Negative_Infinity);
   end Negative_Infinity_Bound;


   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type is
   begin
      return Bound_Type'(Kind       => Concrete_Bound,
                         Comparison => Comparison,
                         Key        => Key);
   end New_Bound;


   function New_Cursor
     (Tree              : Tree_Type;
      Transaction       : Transaction_Type'Class;
      Thread_Safe       : Boolean;
      Lower_Bound       : Bound_Type;
      Upper_Bound       : Bound_Type;
      Reverse_Direction : Boolean := False)
      return Cursor_Type
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Owning_Tree = Tree.Self);
      Direction : Direction_Type;
   begin
      case Reverse_Direction is
         when False =>
            Direction := From_Lower_To_Upper;
         when True =>
            Direction := From_Upper_To_Lower;
      end case;
      return Cursor_Type'(Lower_Bound        => Lower_Bound,
                          Upper_Bound        => Upper_Bound,
                          Direction          => Direction,
                          Owning_Tree        => Tree.Self,
                          Initialized        => True,
                          Thread_Safe        => Thread_Safe,
                          Owning_Transaction =>
                                Nullable_Transaction_Ref_Type(Transaction.Self),
                          others             => <>);
   end New_Cursor;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean)
   is
      pragma Assert (Cursor.Initialized);
   begin
      Lock_Mutex(Cursor);
      Cursor.Thread_Safe := Enabled;
      Unlock_Mutex(Cursor);
   end Set_Thread_Safety;


   procedure Finalize
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Cursor.Owning_Tree = Tree.Self);
   begin
      if Cursor.Initialized then
         Cursor.Initialized := False;
      end if;
   end Finalize;


   procedure Pause
     (Tree        : in out Tree_Type;
      Cursor      : in out Cursor_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Cursor.Initialized);
      pragma Assert (Cursor.Owning_Tree = Tree.Self);
      pragma Assert (Cursor.Owning_Transaction /= null);
   begin
      Lock_Mutex(Cursor);
      Cursor.Owning_Transaction := null;
      Cursor.Force_Recalibrate  := True;
      Unlock_Mutex(Cursor);

   exception
      when others =>
         Unlock_Mutex(Cursor);
         raise;
   end Pause;


   procedure Unpause
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Cursor.Initialized);
      pragma Assert (Cursor.Owning_Tree = Tree.Self);
      pragma Assert (Cursor.Owning_Transaction = null);
   begin
      Lock_Mutex(Cursor);
      Cursor.Owning_Transaction :=
         Nullable_Transaction_Ref_Type(Transaction.Self);
      Cursor.Force_Recalibrate  := True;
      Unlock_Mutex(Cursor);

   exception
      when others =>
         Unlock_Mutex(Cursor);
         raise;
   end Unpause;


   function Key_Matches
     (Left       : Key_Type;
      Comparison : Comparison_Type;
      Right      : Key_Type)
      return Boolean
   is
      pragma Inline (Key_Matches);
   begin
      case Comparison is
         when Less =>
            return not (Right <= Left);
         when Less_Or_Equal =>
            return Left <= Right;
         when Equal =>
            return Left = Right;
         when Greater_Or_Equal =>
            return Right <= Left;
         when Greater =>
            return not (Left <= Right);
      end case;
   end Key_Matches;


   function From_Bound
     (Cursor : Cursor_Type)
      return Bound_Type
   is
      pragma Inline (From_Bound);
   begin
      case Cursor.Direction is
         when From_Lower_To_Upper =>
            return Cursor.Lower_Bound;
         when From_Upper_To_Lower =>
            return Cursor.Upper_Bound;
      end case;
   end From_Bound;


   function To_Bound
     (Cursor : Cursor_Type)
      return Bound_Type
   is
      pragma Inline (To_Bound);
   begin
      case Cursor.Direction is
         when From_Lower_To_Upper =>
            return Cursor.Upper_Bound;
         when From_Upper_To_Lower =>
            return Cursor.Lower_Bound;
      end case;
   end To_Bound;


   function Is_Satisfied
     (Cursor : Cursor_Type;
      Bound  : Bound_Type)
      return Boolean
   is
      pragma Inline (Is_Satisfied);
   begin
      case Bound.Kind is
         when Concrete_Bound =>
            return Key_Matches(Nodes.Key(Cursor.Node, Cursor.Index),
                               Bound.Comparison,
                               Bound.Key);
         when Abstract_Bound =>
            return True;
      end case;
   end Is_Satisfied;


   function Has_Satisfied_Bounds
     (Cursor : Cursor_Type)
      return Boolean
   is
      pragma Inline (Has_Satisfied_Bounds);
   begin
      return Is_Satisfied(Cursor, From_Bound(Cursor)) and
             Is_Satisfied(Cursor, To_Bound(Cursor));
   end Has_Satisfied_Bounds;


   procedure Next
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out Result_Type)
   is

      procedure Move_To_Next
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Cursor      : in out Cursor_Type;
         State       :    out Result_Type)
      is

         procedure Move_To_Lower
           (Tree        : in out Tree_Type;
            Transaction : in out Transaction_Type'Class;
            Cursor      : in out Cursor_Type;
            State       :    out Result_Type)
         is
            pragma Inline (Move_To_Lower);
            use type Nodes.Degree_Type;
         begin
            if Cursor.Index = 1 then
               if Nodes.Is_Valid(Nodes.Left_Neighbor(Cursor.Node)) then
                  declare
                     N   : Nodes.Node_Type renames Cursor.Node;
                     L_A : constant Nodes.Valid_Address_Type
                         := Nodes.To_Valid_Address(Nodes.Left_Neighbor(N));
                     L   : Nodes.Node_Type;
                  begin
                     Read_Node(Tree, Transaction, L_A, L);
                     Cursor.Node  := L;
                     Cursor.Index := Nodes.Degree(L);
                     State        := Success;
                  end;
               else
                  Cursor.Final := True;
                  State        := Success;
               end if;
            else
               Cursor.Index := Cursor.Index - 1;
               State        := Success;
            end if;

         exception
            when others =>
               Cursor.Final := True;
               pragma Warnings (Off);
               State        := Error;
               pragma Warnings (On);
               raise;
         end Move_To_Lower;


         procedure Move_To_Upper
           (Tree        : in out Tree_Type;
            Transaction : in out Transaction_Type'Class;
            Cursor      : in out Cursor_Type;
            State       :    out Result_Type)
         is
            pragma Inline (Move_To_Upper);
            use type Nodes.Degree_Type;
         begin
            if Cursor.Index = Nodes.Degree(Cursor.Node) then
               if Nodes.Is_Valid(Nodes.Right_Neighbor(Cursor.Node)) then
                  declare
                     N   : Nodes.Node_Type renames Cursor.Node;
                     R_A : constant Nodes.Valid_Address_Type
                         := Nodes.To_Valid_Address(Nodes.Right_Neighbor(N));
                     R   : Nodes.Node_Type;
                  begin
                     Read_Node(Tree, Transaction, R_A, R);
                     Cursor.Node  := R;
                     Cursor.Index := 1;
                     State        := Success;
                  end;
               else
                  Cursor.Final := True;
                  State        := Success;
               end if;
            else
               Cursor.Index := Cursor.Index + 1;
               State        := Success;
            end if;

         exception
            when others =>
               Cursor.Final := True;
               pragma Warnings (Off);
               State        := Error;
               pragma Warnings (On);
               raise;
         end Move_To_Upper;

      begin
         case Cursor.Direction is
            when From_Lower_To_Upper =>
               Move_To_Upper(Tree, Transaction, Cursor, State);
            when From_Upper_To_Lower =>
               Move_To_Lower(Tree, Transaction, Cursor, State);
         end case;
      end Move_To_Next;


      procedure Look_Up_Node
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Key         : in     Key_Type;
         Node        :    out Nodes.Node_Type;
         Index       :    out Nodes.Valid_Index_Type;
         State       :    out Result_Type)
      is
         N_A : Nodes.Valid_Address_Type;
         I   : Nodes.Index_Type;
      begin
         N_A := Transaction.Current_Root_Address;
         loop
            declare
               use type Nodes.Degree_Type;
               N : Nodes.Node_Type;
            begin
               Read_Node(Tree, Transaction, N_A, N);
               if Nodes.Degree(N) = 0 then
                  Index := 1;
                  State := Failure;
                  return;
               end if;
               I := Nodes.Key_Position(N, Key);
               if not Nodes.Is_Valid(I) then
                  I := Nodes.Degree(N);
               end if;
               if Nodes.Is_Leaf(N) then
                  Node  := N;
                  Index := I;
                  State := Success;
                  return;
               end if;
               N_A := Nodes.Child(N, I);
            end;
         end loop;

      exception
         when others =>
            Index := 1;
            pragma Warnings (Off);
            State := Error;
            pragma Warnings (On);
            raise;
      end Look_Up_Node;


      procedure Look_Up_Minimum_Node
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Node        :    out Nodes.Node_Type;
         Index       :    out Nodes.Valid_Index_Type;
         State       :    out Result_Type)
      is
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
                  Index := 1;
                  State := Failure;
                  return;
               end if;
               if Nodes.Is_Leaf(N) then
                  Node  := N;
                  Index := 1;
                  State := Success;
                  return;
               end if;
               N_A := Nodes.Child(N, 1);
            end;
         end loop;

      exception
         when others =>
            Index := 1;
            pragma Warnings (Off);
            State := Error;
            pragma Warnings (On);
            raise;
      end Look_Up_Minimum_Node;


      procedure Look_Up_Maximum_Node
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Node        :    out Nodes.Node_Type;
         Index       :    out Nodes.Valid_Index_Type;
         State       :    out Result_Type)
      is
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
                  Index := 1;
                  State := Failure;
                  return;
               end if;
               if Nodes.Is_Leaf(N) then
                  Node  := N;
                  Index := Nodes.Degree(N);
                  State := Success;
                  return;
               end if;
               N_A := Nodes.Child(N, Nodes.Degree(N));
            end;
         end loop;

      exception
         when others =>
            Index := 1;
            pragma Warnings (Off);
            State := Error;
            pragma Warnings (On);
            raise;
      end Look_Up_Maximum_Node;


      procedure Recalibrate
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Cursor      : in out Cursor_Type;
         State       :    out Result_Type) is
      begin
         if Cursor.Final or not Cursor.Has_Node then
            State := Success;
            return;
         end if;

         declare
            Key : constant Key_Type
                := Nodes.Key(Cursor.Node, Cursor.Index);
         begin
            Look_Up_Node(Tree, Transaction, Key, Cursor.Node, Cursor.Index,
                         State);
            if State /= Success then
               Cursor.Final := True;
               return;
            end if;

            -- Move to next key since we don't what to visit one twice.
            case Cursor.Direction is
               when From_Lower_To_Upper =>
                  if Nodes.Key(Cursor.Node, Cursor.Index) <= Key then
                     Move_To_Next(Tree, Transaction, Cursor, State);
                     if Cursor.Final then
                        return;
                     end if;
                  end if;
               when From_Upper_To_Lower =>
                  if Key <= Nodes.Key(Cursor.Node, Cursor.Index) then
                     Move_To_Next(Tree, Transaction, Cursor, State);
                     if Cursor.Final then
                        return;
                     end if;
                  end if;
            end case;

            -- Move on until From_Bound is satisfied.
            loop
               if Is_Satisfied(Cursor, From_Bound(Cursor)) then
                  return;
               end if;
               Move_To_Next(Tree, Transaction, Cursor, State);
               if Cursor.Final then
                  return;
               end if;
               if not Is_Satisfied(Cursor, To_Bound(Cursor)) then
                  Cursor.Final := True;
                  return;
               end if;
            end loop;
         end;
      end Recalibrate;


      procedure Look_Up_Abstract_From_Bound
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Cursor      : in out Cursor_Type;
         State       :    out Result_Type)
      is
         pragma Inline (Look_Up_Abstract_From_Bound);
         pragma Assert (not Cursor.Has_Node);
      begin
         case From_Bound(Cursor).Location is
            when Negative_Infinity =>
               Look_Up_Minimum_Node(Tree, Transaction, Cursor.Node,
                                    Cursor.Index, State);
               if State /= Success then
                  Cursor.Final := True;
                  return;
               end if;
            when Positive_Infinity =>
               Look_Up_Maximum_Node(Tree, Transaction, Cursor.Node,
                                    Cursor.Index, State);
               if State /= Success then
                  Cursor.Final := True;
                  return;
               end if;
         end case;
      end Look_Up_Abstract_From_Bound;


      procedure Look_Up_Concrete_From_Bound
        (Tree        : in out Tree_Type;
         Transaction : in out Transaction_Type'Class;
         Cursor      : in out Cursor_Type;
         State       :    out Result_Type)
      is
         pragma Inline (Look_Up_Concrete_From_Bound);
         pragma Assert (not Cursor.Has_Node);
         FB : constant Bound_Type := From_Bound(Cursor);
      begin
         Look_Up_Node(Tree, Transaction, FB.Key, Cursor.Node, Cursor.Index,
                      State);
         if State /= Success then
            Cursor.Final := True;
            return;
         end if;

         declare
            K : constant Key_Type := Nodes.Key(Cursor.Node, Cursor.Index);
         begin
            case FB.Comparison is
               when Less | Less_Or_Equal | Greater =>
                  if not Key_Matches(K, FB.Comparison, FB.Key) then
                     Move_To_Next(Tree, Transaction, Cursor, State);
                  else
                     State := Success;
                  end if;
               when Greater_Or_Equal =>
                  State := Success;
               when Equal =>
                  if not Key_Matches(K, FB.Comparison, FB.Key) then
                     Cursor.Final := True;
                     State        := Failure;
                  else
                     State := Success;
                  end if;
            end case;
         end;
      end Look_Up_Concrete_From_Bound;


      procedure Initialize_Output_If_Successful_And_Bounds_Satisfied
        (Cursor : in     Cursor_Type;
         State  : in out Result_Type;
         Key    :    out Key_Type;
         Value  :    out Value_Type)
      is
         pragma Inline (Initialize_Output_If_Successful_And_Bounds_Satisfied);
      begin
         if (not Cursor.Final and State = Success) and then
         Has_Satisfied_Bounds(Cursor) then
            State := Success;
            Key   := Nodes.Key(Cursor.Node, Cursor.Index);
            Value := Nodes.Value(Cursor.Node, Cursor.Index);
         elsif State = Success then
            State := Failure;
         end if;
      end Initialize_Output_If_Successful_And_Bounds_Satisfied;

      pragma Assert (Tree.Initialized);
      pragma Assert (Cursor.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Started);
      pragma Assert (Cursor.Owning_Tree = Tree.Self);
      pragma Assert (Cursor.Owning_Transaction =
                     Nullable_Transaction_Ref_Type(Transaction.Self));
      use type Nodes.Degree_Type;
   begin
      Lock_Mutex(Cursor);
      if Cursor.Final then
         State := Failure;
         Unlock_Mutex(Cursor);
         return;
      end if;

      if not Cursor.Has_Node then
         case From_Bound(Cursor).Kind is
            when Concrete_Bound =>
               Look_Up_Concrete_From_Bound(Tree, Transaction, Cursor, State);
            when Abstract_Bound =>
               Look_Up_Abstract_From_Bound(Tree, Transaction, Cursor, State);
         end case;
         Cursor.Has_Node := (State = Success);
      elsif Cursor.Force_Recalibrate then
         Recalibrate(Tree, Transaction, Cursor, State);
         Cursor.Force_Recalibrate := (State = Success);
      else
         Move_To_Next(Tree, Transaction, Cursor, State);
      end if;
      Initialize_Output_If_Successful_And_Bounds_Satisfied(Cursor, State,
                                                           Key, Value);
      Unlock_Mutex(Cursor);

   exception
      when others =>
         Unlock_Mutex(Cursor);
         raise;
   end Next;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Cursor.Initialized);
      pragma Assert (Cursor.Owning_Tree = Tree.Self);
      Sub_Transaction : Sub_RW_Transaction_Type
                      := New_Sub_RW_Transaction(Tree, Transaction);
   begin
      Lock_Mutex(Cursor);
      if not Cursor.Has_Node then
         Position := Count_Type'First;
         State := Failure;
         Unlock_Mutex(Cursor);
         return;
      end if;
      Key := Nodes.Key(Cursor.Node, Cursor.Index);
      declare
      begin
         Start_Transaction(Tree, Sub_Transaction);
         Delete(Tree, Sub_Transaction, Key, Value, Position, State);
         if State /= Success then
            Abort_Transaction(Tree, Sub_Transaction);
         else
            Commit_Transaction(Tree, Sub_Transaction);
            Cursor.Force_Recalibrate := True;
         end if;
      exception
         when others =>
            Abort_Transaction(Tree, Sub_Transaction);
            Unlock_Mutex(Cursor);
            raise;
      end;
      Unlock_Mutex(Cursor);

   exception
      when others =>
         Unlock_Mutex(Cursor);
         raise;
   end Delete;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Cursor.Initialized);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Started);
      pragma Assert (Cursor.Owning_Tree = Tree.Self);
      pragma Assert (Cursor.Owning_Transaction =
                     Nullable_Transaction_Ref_Type(Transaction.Self));
   begin
      Lock_Mutex(Cursor);
      if not Cursor.Has_Node then
         Position := Count_Type'First;
         State := Failure;
         Unlock_Mutex(Cursor);
         return;
      end if;
      Key := Nodes.Key(Cursor.Node, Cursor.Index);
      Delete(Tree, Transaction, Key, Value, Position, State);
      Cursor.Force_Recalibrate := True;
      Unlock_Mutex(Cursor);

   exception
      when others =>
         Unlock_Mutex(Cursor);
         raise;
   end Delete;

end Cursors;

