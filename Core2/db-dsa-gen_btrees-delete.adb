-- Abstract:
--
-- Deletes a new Key/Value pair as follows.
-- (1) Descend to a leaf using the Stacks package.
-- (2) Move to the right as until a node is reached that either contains the
--     searched key or guarantees that there is no node that contains the key.
--     Remove the entry from the found leaf.
-- (3) Now start the ascent according to the Stacks package.
--
-- Design Nodes:
--
-- Since the insertion heavily relies on the nested Stacks package, check its
-- documentation for information about locking etc.
--
-- Why don't we just remove the entry from the leaf and then stop?
-- The ascent is necessary to ensure that the keys of the pointers in the
-- parents are tight.
-- The necessity of this is shown by the following example. Assume that we only
-- remove the entry from the leaves as described in [L&Y] and that our tree
-- allows duplicate keys.
-- Let's say we have a node P=(3,6) with the childen N=(1,2,3,[h=3]) and
-- M=(4,5,6,[h=6]).
-- Then delete everything in reverse order to obtain P=(3,6) and N=([h=1]) and
-- M=([h=4]).
-- Then insert three 1s to get P=(3,6) and N=(1,1,1,[h=1]) and M=([h=4]).
-- Then insert a 2 to obtain P=(3,6) and N=(1,1,1,[h=1]) and M=(2,[h=2]).
-- Although the tree is still functioning, it is obviously not intended that M
-- contains elements greater than the key of the pointer to N in P (2 < 3).
-- This gets really serious if later M is split which will lead to P=(3,2,...).
--
-- Now one could say that updating the key in the parent is only necessary in
-- the nodes of the outermost-right path, and only if the high key has become
-- greater. This doesn't work either:
-- Assume that now a 1 is deleted to get P=(3,6) and N=(1,1,[h=1]) and
-- M=(2,[h=2]).
-- Then insert a 3 to get P=(3,6) and N=(1,1,3,[h=3]) and M=(2,[h=2]).
-- Now the tree is messed up because the M contains items less than the high key
-- of N.
--
-- A really different approach to (probably) fix this problem would be to really
-- physically store the high keys and change the semantics of the high key to
-- "the greatest key ever stored in the node". But this would waste pretty much
-- space.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.DSA.Gen_BTrees)
procedure Delete
  (Tree     : in out Tree_Type;
   Key      : in     Keys.Key_Type;
   Value    :    out Values.Value_Type;
   State    :    out State_Type)
is
   pragma Precondition (Tree.Initialized);

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
         High_Key     : Keys.Key_Type;
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
      raise;
end Delete;

