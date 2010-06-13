-- Abstract:
--
-- Inserts a new Key/Value pair as follows.
-- (1) Descend to a leaf using the Stacks package.
-- (2) Move to the right as until a node is reached whose high key allows us to
--     insert the entry. If the high key equals the key of the entry, the entry
--     is only inserted if the node has enough space for it; otherwise the
--     procedure proceeds to the next node.
--     Insert the entry into the found leaf.
-- (3) Now start the ascent according to the Stacks package.
--
-- Design Nodes:
--
-- Since the insertion heavily relies on the nested Stacks package, check its
-- documentation for information about locking etc.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.DSA.Gen_BTrees)
procedure Insert
  (Tree             : in out Tree_Type;
   Key              : in     Keys.Key_Type;
   Value            : in     Values.Value_Type;
   Allow_Duplicates : in     Boolean;
   State            :    out State_Type)
is
   pragma Precondition (Tree.Initialized);

   Stack : Stacks.Stack_Type;

   use type Nodes.Valid_Index_Type;
   use type Blocks.Size_Type;
   N_A   : Nodes.Valid_Address_Type;
   N_Old : Nodes.RW_Node_Type;
begin
   if Keys.Size_Bound (Key) > Max_Key_Size (Values.Size_Bound (Value)) then
      State := Failure;
      return;
   end if;

   Stacks.Initialize (Stack, Key);

   declare
      use type Nodes.Level_Type;

      function Exit_Cond (N : Nodes.Node_Type) return Boolean
      is
         function Fits_Into_Node
           (K : Keys.Key_Type;
            V : Values.Value_Type)
            return Boolean
         is
            use type Nodes.Validation_State_Type;
            I : Nodes.Index_Type;
         begin
           I := Nodes.Key_Position (N, K);
           if not Nodes.Is_Valid (I) then
              I := Nodes.Degree (N) + 1;
           end if;
           return Nodes.Validation (Nodes.Insertion (N, I, K, V)) /=
                  Nodes.Too_Large;
         end Fits_Into_Node;
      begin
         if not Nodes.Is_Leaf (N) then
            return True;
         end if;
         if not Nodes.Is_Valid (Nodes.Link (N)) then
            return True;
         end if;
         case Keys.Compare (Key, Nodes.High_Key (N)) is
            when Utils.Less    => return True;
            when Utils.Equal   => return Fits_Into_Node (Key, Value);
            when Utils.Greater => return False;
         end case;
      end Exit_Cond;
   begin
      Stacks.Build_Stack (Tree, Stack, Nodes.Leaf_Level);
      Stacks.Pop (Stack, N_A);
      Stacks.Move_Right (Tree, Stack, Nodes.Leaf_Level, Exit_Cond'Access,
                         N_A, N_Old);
   end;

   declare
      N : Nodes.RW_Node_Type;
      I : Nodes.Index_Type;
   begin
      I := Nodes.Key_Position (N_Old, Key);
      if not Nodes.Is_Valid (I) then
         I := Nodes.Degree (N_Old) + 1;
      elsif not Allow_Duplicates and then Nodes.Key (N_Old, I) = Key then
         Stacks.Finalize (Stack);
         State := Failure;
         return;
      end if;
      N := Nodes.Insertion (N_Old, I, Key, Value);
      Stacks.Write_And_Ascend (Tree, Stack, N_A, N_Old, N);
      State := Success;
   end;

   Stacks.Finalize (Stack);

exception
   when others =>
      Stacks.Finalize (Stack);
      raise;
end Insert;

