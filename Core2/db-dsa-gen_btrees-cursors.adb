-- Abstract:
--
-- see spec
--
-- Design Notes:
--
-- In the Next operation, the State variable has multiple meanings. At the
-- outermost level, i.e. not in the nested subprograms, State = Success means
-- that there exists a next Key/Value pair. In the nested subprograms however,
-- State = Success means that the subprogram finished successfully.
--
-- An example: in the nested Move_To_Next procedure in the case that we've
-- surpassed the last Key in the tree, it sets Final := True and State :=
-- Success. This means that Move_To_Next worked correctly and as expected, but
-- that the final value of State will be Failure because there is no next
-- Key/Value pair.
--
-- Locking the leaves seems to be a good idea to enforce that the following
-- invariant holds: the previous key is less than or equal to the current key.
-- However, this is given even without locking, because if the currently visited
-- node is split during the visit, the cursor would just continue at the node
-- which has been the right neighbor before the split. It thereby might skip
-- some new entries, but the order would be consistent.
--
-- Copyright 2008--2011 Christoph Schwering

separate (DB.DSA.Gen_BTrees)
package body Cursors is

   procedure Lock_Mutex
     (Cursor : in out Cursor_Type) is
   begin
      if Cursor.Thread_Safe then
         Locks.Mutexes.Lock (Cursor.Mutex);
      end if;
   end Lock_Mutex;


   procedure Unlock_Mutex
     (Cursor : in out Cursor_Type) is
   begin
      if Cursor.Thread_Safe then
         Locks.Mutexes.Unlock (Cursor.Mutex);
      end if;
   end Unlock_Mutex;


   function Positive_Infinity_Bound
      return Bound_Type is
   begin
      return Bound_Type'(Kind => Abstract_Bound,
                         Location => Positive_Infinity);
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound
      return Bound_Type is
   begin
      return Bound_Type'(Kind => Abstract_Bound,
                         Location => Negative_Infinity);
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
      pragma Precondition (Tree.Initialized);

      procedure Check_Bounds is
      begin
         case Lower_Bound.Kind is
            when Abstract_Bound =>
               if Lower_Bound.Location = Positive_Infinity then
                  raise Tree_Error;
               end if;
            when Concrete_Bound =>
               if Lower_Bound.Comparison = Less_Or_Equal or
                  Lower_Bound.Comparison = Less
               then
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
                  Upper_Bound.Comparison = Greater
               then
                  raise Tree_Error;
               end if;
         end case;
      end Check_Bounds;
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
      pragma Precondition (Cursor.Initialized);
   begin
      Lock_Mutex (Cursor);
      Cursor.Thread_Safe := Enabled;
      Unlock_Mutex (Cursor);
   end Set_Thread_Safety;


   procedure Pause
     (Tree   : in     Tree_Type;
      Cursor : in out Cursor_Type)
   is
      pragma Precondition (Tree.Initialized);
      pragma Precondition (Cursor.Initialized);
      pragma Precondition (Cursor.Owning_Tree = Tree.Self);
   begin
      Lock_Mutex (Cursor);
      Cursor.Force_Recalibrate := True;
      Unlock_Mutex (Cursor);

   exception
      when others =>
         Unlock_Mutex (Cursor);
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
            return Key_Matches (Cursor.Key, Bound.Comparison, Bound.Key);
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
      return Is_Satisfied (Cursor, Cursor.Lower_Bound) and
             Is_Satisfied (Cursor, Cursor.Upper_Bound);
   end Has_Satisfied_Bounds;


   procedure Next
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Keys.Key_Type;
      Value  :    out Values.Value_Type;
      State  :    out State_Type)
   is
      pragma Precondition (Tree.Initialized);
      pragma Precondition (Cursor.Initialized);
      pragma Precondition (Cursor.Owning_Tree = Tree.Self);

      procedure Init_Contexts_And_Key is
      begin
         Cursor.Key_Context   := Keys.New_Read_Context;
         Cursor.Value_Context := Values.New_Read_Context;
         Nodes.Get_Key
           (Cursor.Node, Cursor.Index, Cursor.Key, Cursor.Key_Context);
      end Init_Contexts_And_Key;


      procedure Move_To_Next
      is
         use type Nodes.Degree_Type;
      begin
         if Cursor.Index = Nodes.Degree (Cursor.Node) then
            loop
               if Nodes.Is_Valid (Nodes.Link (Cursor.Node)) then
                  Read_Node (Tree, Nodes.Valid_Link (Cursor.Node), Cursor.Node);
                  if Nodes.Degree (Cursor.Node) > 0 then
                     Cursor.Index := 1;
                     Init_Contexts_And_Key;
                     State := Success;
                     exit;
                  end if;
               else
                  Cursor.Final := True;
                  State := Success;
                  exit;
               end if;
            end loop;
         else
            Cursor.Index := Cursor.Index + 1;
            Nodes.Get_Key
              (Cursor.Node, Cursor.Index, Cursor.Key, Cursor.Key_Context);
            State := Success;
         end if;
      exception
         when others =>
            Cursor.Final := True;
            raise;
      end Move_To_Next;


      procedure Recalibrate is
      begin
         if Cursor.Final or not Cursor.Has_Node then
            State := Success;
            return;
         end if;

         declare
            use type Nodes.Degree_Type;
            Old_Key : constant Keys.Key_Type := Cursor.Key;
         begin
            Searches.Search_Node
              (Tree, Old_Key, Cursor.Node, Cursor.Index, State);

            if Nodes.Is_Valid (Cursor.Index) then
               State := Success;
            else
               -- Move right until a non-empty node is found.
               loop
                  if Nodes.Degree (Cursor.Node) > 0 then
                     Cursor.Index := 1;
                     State := Success;
                     exit;
                  end if;
                  if not Nodes.Is_Valid (Nodes.Link (Cursor.Node)) then
                     State := Failure;
                     exit;
                  end if;
                  Read_Node (Tree, Nodes.Valid_Link (Cursor.Node), Cursor.Node);
               end loop;
            end if;

            if State /= Success then
               Cursor.Final := True;
               return;
            end if;
            Init_Contexts_And_Key;

            -- Move to next key since we don't what to visit one twice.
            while Cursor.Key <= Old_Key loop
               Move_To_Next;
               if Cursor.Final then
                  return;
               end if;
            end loop;

            -- Move on until Lower_Bound is satisfied.
            while not Is_Satisfied (Cursor, Cursor.Lower_Bound) loop
               Move_To_Next;
               if Cursor.Final then
                  return;
               end if;
               if not Is_Satisfied (Cursor, Cursor.Upper_Bound) then
                  Cursor.Final := True;
                  return;
               end if;
            end loop;
         end;
      end Recalibrate;


      procedure Search_Abstract_Lower_Bound
      is
         pragma Precondition (not Cursor.Has_Node);
      begin
         case Cursor.Lower_Bound.Location is
            when Negative_Infinity =>
               Searches.Search_Minimum_Node
                 (Tree, Cursor.Node, Cursor.Index, State);
               if State /= Success then
                  Cursor.Final := True;
                  return;
               end if;
            when Positive_Infinity =>
               raise Tree_Error;
         end case;
         Init_Contexts_And_Key;
      end Search_Abstract_Lower_Bound;


      procedure Search_Concrete_Lower_Bound
      is
         pragma Precondition (not Cursor.Has_Node);
         pragma Precondition (Cursor.Lower_Bound.Comparison /= Less);
         pragma Precondition (Cursor.Lower_Bound.Comparison /= Less_Or_Equal);
         use type Nodes.Degree_Type;
      begin
         Searches.Search_Node
           (Tree, Cursor.Lower_Bound.Key, Cursor.Node, Cursor.Index, State);

         if Nodes.Is_Valid (Cursor.Index) then
            State := Success;
         else
            -- Move right until a non-empty node is found.
            loop
               if Nodes.Degree (Cursor.Node) > 0 then
                  Cursor.Index := 1;
                  State := Success;
                  exit;
               end if;
               if not Nodes.Is_Valid (Nodes.Link (Cursor.Node)) then
                  State := Failure;
                  exit;
               end if;
               Read_Node (Tree, Nodes.Valid_Link (Cursor.Node), Cursor.Node);
            end loop;
         end if;

         if State /= Success then
            Cursor.Final := True;
            return;
         end if;
         Init_Contexts_And_Key;

         case Cursor.Lower_Bound.Comparison is
            when Less | Less_Or_Equal =>
               raise Tree_Error;
            when Equal =>
               if Cursor.Key = Cursor.Lower_Bound.Key then
                  State := Success;
               else
                  State := Failure;
               end if;
            when Greater_Or_Equal | Greater =>
               while not Is_Satisfied (Cursor, Cursor.Lower_Bound) loop
                  Move_To_Next;
               end loop;
         end case;
      end Search_Concrete_Lower_Bound;

      use type Nodes.Degree_Type;
   begin
      Lock_Mutex (Cursor);
      if Cursor.Final then
         State := Failure;
         Unlock_Mutex (Cursor);
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
         Has_Satisfied_Bounds (Cursor)
      then
         State := Success;
         Key   := Cursor.Key;
         Nodes.Get_Value
           (Cursor.Node, Cursor.Index, Value, Cursor.Key_Context,
            Cursor.Value_Context);
      elsif State = Success then
         State := Failure;
      end if;
      Unlock_Mutex (Cursor);

   exception
      when others =>
         Unlock_Mutex (Cursor);
         raise;
   end Next;


   procedure Delete
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type;
      State  :    out State_Type)
   is
      pragma Precondition (Tree.Initialized);
      pragma Precondition (Cursor.Initialized);
      pragma Precondition (Cursor.Owning_Tree = Tree.Self);

      Value : Values.Value_Type;
   begin
      Lock_Mutex (Cursor);
      if Cursor.Final or not Cursor.Has_Node then
         State := Failure;
         Unlock_Mutex (Cursor);
         return;
      end if;

      Delete (Tree, Cursor.Key, Value, State);
      Cursor.Force_Recalibrate := True;
      Unlock_Mutex (Cursor);

   exception
      when others =>
         Unlock_Mutex (Cursor);
         raise;
   end Delete;

end Cursors;

