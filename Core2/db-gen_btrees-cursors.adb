-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

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
      Key        : Keys.Key_Type)
      return Bound_Type is
   begin
      return Bound_Type'(Kind       => Concrete_Bound,
                         Comparison => Comparison,
                         Key        => Key);
   end New_Bound;


   function New_Cursor
     (Tree        : Tree_Type;
      Thread_Safe : Boolean;
      Lower_Bound : Bound_Type;
      Upper_Bound : Bound_Type)
      return Cursor_Type
   is
      procedure Check_Bounds is
      begin
         case Lower_Bound.Kind is
            when Abstract_Bound =>
               if Lower_Bound.Location = Positive_Infinity then
                  raise Tree_Error;
               end if;
            when Concrete_Bound =>
               if Lower_Bound.Comparison = Less_Or_Equal or
                  Lower_Bound.Comparison = Less then
                  raise Tree_Error;
               end if;
         end case;

         case Upper_Bound.Kind is
            when Abstract_Bound =>
               if Upper_Bound.Location = Negative_Infinity then
                  raise Tree_Error;
               end if;
            when Concrete_Bound =>
               if Upper_Bound.Comparison = Greater_Or_Equal or
                  Upper_Bound.Comparison = Greater then
                  raise Tree_Error;
               end if;
         end case;
      end Check_Bounds;

      pragma Assert (Tree.Initialized);
   begin
      Check_Bounds;
      return Cursor_Type'(Lower_Bound => Lower_Bound,
                          Upper_Bound => Upper_Bound,
                          Owning_Tree => Tree.Self,
                          Initialized => True,
                          Thread_Safe => Thread_Safe,
                          others      => <>);
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


   procedure Finalize_Cursor
     (Tree   : in     Tree_Type;
      Cursor : in out Cursor_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Cursor.Owning_Tree = Tree.Self);
   begin
      if Cursor.Initialized then
         Cursor.Initialized := False;
      end if;
   end Finalize_Cursor;


   procedure Pause
     (Tree   : in     Tree_Type;
      Cursor : in out Cursor_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Cursor.Initialized);
      pragma Assert (Cursor.Owning_Tree = Tree.Self);
   begin
      Lock_Mutex(Cursor);
      Cursor.Force_Recalibrate := True;
      Unlock_Mutex(Cursor);

   exception
      when others =>
         Unlock_Mutex(Cursor);
         raise;
   end Pause;


   function Key_Matches
     (Left       : Keys.Key_Type;
      Comparison : Comparison_Type;
      Right      : Keys.Key_Type)
      return Boolean
   is
      pragma Inline (Key_Matches);
      use type Utils.Comparison_Result_Type;
   begin
      case Comparison is
         when Less             => return Left < Right;
         when Less_Or_Equal    => return Left <= Right;
         when Equal            => return Left = Right;
         when Greater_Or_Equal => return Left >= Right;
         when Greater          => return Left > Right;
      end case;
   end Key_Matches;


   function Is_Satisfied
     (Cursor : Cursor_Type;
      Bound  : Bound_Type)
      return Boolean
   is
      pragma Inline (Is_Satisfied);
   begin
      case Bound.Kind is
         when Concrete_Bound =>
            return Key_Matches(Cursor.Key, Bound.Comparison, Bound.Key);
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
      return Is_Satisfied(Cursor, Cursor.Lower_Bound) and
             Is_Satisfied(Cursor, Cursor.Upper_Bound);
   end Has_Satisfied_Bounds;


   procedure Next
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Keys.Key_Type;
      Value  :    out Values.Value_Type;
      State  :    out State_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Cursor.Initialized);
      pragma Assert (Cursor.Owning_Tree = Tree.Self);

      procedure Move_To_Next
      is
         use type Nodes.Degree_Type;
      begin
         if Cursor.Index = Nodes.Degree(Cursor.Node) then
            if Nodes.Is_Valid(Nodes.Link(Cursor.Node)) then
               declare
                  N   : Nodes.RO_Node_Type renames Cursor.Node;
                  R_A : constant Nodes.Valid_Address_Type :=
                     Nodes.To_Valid_Address(Nodes.Link(N));
                  R   : Nodes.RO_Node_Type;
               begin
                  Read_Node(Tree, R_A, R);
                  Cursor.Node          := R;
                  Cursor.Key_Context   := Keys.New_Read_Context;
                  Cursor.Value_Context := Values.New_Read_Context;
                  Cursor.Index         := 1;
                  Nodes.Get_Key(Cursor.Node, Cursor.Index, Cursor.Key,
                                Cursor.Key_Context);
                  State := Success;
               end;
            else
               Cursor.Final := True;
               State        := Success;
            end if;
         else
            Cursor.Index := Cursor.Index + 1;
            Nodes.Get_Key(Cursor.Node, Cursor.Index, Cursor.Key,
                          Cursor.Key_Context);
            State := Success;
         end if;

      exception
         when others =>
            Cursor.Final := True;
            pragma Warnings (Off);
            State := Error;
            pragma Warnings (On);
            raise;
      end Move_To_Next;


      procedure Search_Node
        (Key   : in  Keys.Key_Type;
         Node  : out Nodes.RO_Node_Type;
         Index : out Nodes.Valid_Index_Type)
      is
         N_A : Nodes.Valid_Address_Type := Root_Address;
      begin
         loop
            declare
               use type Nodes.Degree_Type;
               N : Nodes.RO_Node_Type;
               I : Nodes.Index_Type;
            begin
               Read_Node(Tree, N_A, N);
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
      end Search_Node;


      procedure Recalibrate is
      begin
         if Cursor.Final or not Cursor.Has_Node then
            State := Success;
            return;
         end if;

         declare
            Old_Key : constant Keys.Key_Type := Cursor.Key;
         begin
            Search_Node(Old_Key, Cursor.Node, Cursor.Index);
            if State /= Success then
               Cursor.Final := True;
               return;
            end if;
            Cursor.Key_Context   := Keys.New_Read_Context;
            Cursor.Value_Context := Values.New_Read_Context;
            Nodes.Get_Key(Cursor.Node, Cursor.Index, Cursor.Key,
                          Cursor.Key_Context);

            -- Move to next key since we don't what to visit one twice.
            if Cursor.Key <= Old_Key then
               Move_To_Next;
               if Cursor.Final then
                  return;
               end if;
            end if;

            -- Move on until Lower_Bound is satisfied.
            loop
               if Is_Satisfied(Cursor, Cursor.Lower_Bound) then
                  return;
               end if;
               Move_To_Next;
               if Cursor.Final then
                  return;
               end if;
               if not Is_Satisfied(Cursor, Cursor.Upper_Bound) then
                  Cursor.Final := True;
                  return;
               end if;
            end loop;
         end;
      end Recalibrate;


      procedure Search_Abstract_Lower_Bound
      is
         procedure Search_Minimum_Node
           (Node  : out Nodes.RO_Node_Type;
            Index : out Nodes.Valid_Index_Type)
         is
            N_A : Nodes.Valid_Address_Type := Root_Address;
         begin
            loop
               declare
                  use type Nodes.Degree_Type;
                  N : Nodes.RO_Node_Type;
               begin
                  Read_Node(Tree, N_A, N);
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
         end Search_Minimum_Node;

         procedure Search_Maximum_Node
           (Node  : out Nodes.RO_Node_Type;
            Index : out Nodes.Valid_Index_Type)
         is
            N_A : Nodes.Valid_Address_Type := Root_Address;
         begin
            loop
               declare
                  use type Nodes.Degree_Type;
                  N : Nodes.RO_Node_Type;
               begin
                  Read_Node(Tree, N_A, N);
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
         end Search_Maximum_Node;

         pragma Assert (not Cursor.Has_Node);
      begin
         case Cursor.Lower_Bound.Location is
            when Negative_Infinity =>
               Search_Minimum_Node(Cursor.Node, Cursor.Index);
               if State /= Success then
                  Cursor.Final := True;
                  return;
               end if;
            when Positive_Infinity =>
               Search_Maximum_Node(Cursor.Node, Cursor.Index);
               if State /= Success then
                  Cursor.Final := True;
                  return;
               end if;
         end case;
      end Search_Abstract_Lower_Bound;


      procedure Search_Concrete_Lower_Bound
      is
         pragma Assert (not Cursor.Has_Node);
         FB : constant Bound_Type := Cursor.Lower_Bound;
      begin
         Search_Node(FB.Key, Cursor.Node, Cursor.Index);
         if State /= Success then
            Cursor.Final := True;
            return;
         end if;

         case FB.Comparison is
            when Less | Less_Or_Equal =>
               pragma Assert (False);
               null;
            when Equal | Greater =>
               if not Key_Matches(Cursor.Key, FB.Comparison, FB.Key) then
                  Move_To_Next;
               else
                  State := Success;
               end if;
            when Greater_Or_Equal =>
               State := Success;
         end case;
      end Search_Concrete_Lower_Bound;

      use type Nodes.Degree_Type;
   begin
      Lock_Mutex(Cursor);
      if Cursor.Final then
         State := Failure;
         Unlock_Mutex(Cursor);
         return;
      end if;

      if not Cursor.Has_Node then
         case Cursor.Lower_Bound.Kind is
            when Concrete_Bound =>
               Search_Concrete_Lower_Bound;
            when Abstract_Bound =>
               Search_Abstract_Lower_Bound;
         end case;
         Cursor.Has_Node := (State = Success);
      elsif Cursor.Force_Recalibrate then
         Recalibrate;
         Cursor.Force_Recalibrate := (State = Success);
      else
         Move_To_Next;
      end if;

      if (not Cursor.Final and State = Success) and then
         Has_Satisfied_Bounds(Cursor) then
         State := Success;
         Key   := Cursor.Key;
         Nodes.Get_Value(Cursor.Node, Cursor.Index, Value,
                         Cursor.Key_Context, Cursor.Value_Context);
      elsif State = Success then
         State := Failure;
      end if;
      Unlock_Mutex(Cursor);

   exception
      when others =>
         Unlock_Mutex(Cursor);
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Next;


   procedure Delete
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Keys.Key_Type;
      Value  :    out Values.Value_Type;
      State  :    out State_Type)
   is
      pragma Assert (Tree.Initialized);
      pragma Assert (Cursor.Initialized);
      pragma Assert (Cursor.Owning_Tree = Tree.Self);
   begin
      Lock_Mutex(Cursor);
      if not Cursor.Has_Node then
         State := Failure;
         Unlock_Mutex(Cursor);
         return;
      end if;
      Key := Cursor.Key;
      Nodes.Get_Value(Cursor.Node, Cursor.Index, Value, Cursor.Key_Context,
                      Cursor.Value_Context);
      Delete(Tree, Key, Value, State);
      Cursor.Force_Recalibrate := True;
      Unlock_Mutex(Cursor);

   exception
      when others =>
         Unlock_Mutex(Cursor);
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Delete;

end Cursors;

