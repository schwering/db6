-- Abstract:
--
-- The search descends to a leaf and then moves right as long as there is hope
-- to find the key.
-- During all this time, nothing is locked.
--
-- Design Notes:
--
-- The semantics of the Search_Node procedure are a little bit tricky because it
-- is used for the Search and Ceiling operations and Cursor's Search_Lower_Bound
-- and Recalibrate. While in the first case, we want an exact match and only and
-- exact match, in the other cases we want the minimum existent key that is
-- equal to or greater than the parameter Key -- i.e. the ceiling. From this
-- position on we move to the right to satisfy the condition.
--
-- Care needs to be taken to handle empty nodes correctly. For this reason,
-- Search_Node still sets State to Success only if an exact match is found, but
-- it also sets Index if State = Failure:
-- If Index is valid, i.e. in 1 .. degree of node, it has found a greater
-- (State = Failure) or equal (State = Success) key. Otherwise, if Index is not
-- valid, it has determined that there is no node that contains the parameter
-- Key. This implies that the minimum greater key lies in a node right from the
-- current node if it even exists.
--
-- Copyright 2008--2011 Christoph Schwering

separate (DB.DSA.Gen_BTrees)
package body Searches is

   procedure Search_Node
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      N     :    out Nodes.Node_Type;
      Index :    out Nodes.Extended_Index_Type;
      State :    out State_Type)
   is
      pragma Precondition (Tree.Initialized);

      procedure Find_Leaf;

      procedure Find_Leaf
      is
         pragma Postcondition (Nodes.Is_Leaf (N));
         pragma Inline (Find_Leaf);
         N_A : Nodes.Valid_Address_Type := Root_Address;
      begin
         loop
            Read_Node (Tree, N_A, N);
            exit when Nodes.Is_Leaf (N);
            N_A := Scan_Node (N, Key);
         end loop;
      end Find_Leaf;

      use type Utils.Comparison_Result_Type;
   begin
      Find_Leaf;
      loop
         Index := Nodes.Key_Position (N, Key);
         if Nodes.Is_Valid (Index) then
            case Keys.Compare (Key, Nodes.Key (N, Index)) is
               when Utils.Less =>
                  State := Failure;
                  return;
               when Utils.Equal =>
                  State := Success;
                  return;
               when Utils.Greater =>
                  raise Tree_Error;
            end case;
         elsif not Nodes.Is_Valid (Nodes.Link (N)) then
            State := Failure;
            return;
         else
            declare
               High_Key     : Keys.Key_Type;
               Has_High_Key : Boolean;
            begin
               Nodes.Get_High_Key (N, High_Key, Has_High_Key);
               if Has_High_Key and then Key < High_Key then
                  State := Failure;
                  return;
               end if;
            end;
            Read_Node (Tree, Nodes.Valid_Link (N), N);
         end if;
      end loop;
   end Search_Node;


   procedure Search
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type)
   is
      N : Nodes.RO_Node_Type;
      I : Nodes.Extended_Index_Type;
   begin
      Search_Node (Tree, Key, N, I, State);
      if State = Success then
         Value := Nodes.Value (N, I);
      end if;
   end Search;


   procedure Ceiling
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Ceil  :    out Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type)
   is
      use type Utils.Comparison_Result_Type;
      N : Nodes.RO_Node_Type;
      I : Nodes.Extended_Index_Type;
   begin
      Search_Node (Tree, Key, N, I, State);
      if Nodes.Is_Valid (I) then
         Ceil  := Nodes.Key (N, I);
         Value := Nodes.Value (N, I);
         State := Success;
      else
         -- Move right until a non-empty node is found.
         loop
            if not Nodes.Is_Valid (Nodes.Link (N)) then
               State := Failure;
               exit;
            end if;
            Read_Node (Tree, Nodes.Valid_Link (N), N);
            I := Nodes.Key_Position (N, Key);
            if Nodes.Is_Valid (I) then
               Ceil  := Nodes.Key (N, I);
               Value := Nodes.Value (N, I);
               State := Success;
               exit;
            end if;
         end loop;
      end if;
   end Ceiling;


   procedure Search_Minimum_Node
     (Tree  : in out Tree_Type;
      N     :    out Nodes.Node_Type;
      Index :    out Nodes.Index_Type;
      State :    out State_Type)
   is
      pragma Precondition (Tree.Initialized);
      use type Nodes.Degree_Type;
      N_A : Nodes.Valid_Address_Type := Root_Address;
   begin
      loop
         Read_Node (Tree, N_A, N);
         if Nodes.Is_Inner (N) then
            N_A := Nodes.Child (N, 1);
         else
            if Nodes.Degree (N) > 0 then
               Index := 1;
               State := Success;
               return;
            elsif Nodes.Is_Valid (Nodes.Link (N)) then
               N_A := Nodes.Valid_Link (N);
            else
               State := Failure;
               return;
            end if;
         end if;
      end loop;
   end Search_Minimum_Node;


   procedure Search_Minimum
     (Tree  : in out Tree_Type;
      Key   :    out Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type)
   is
      N : Nodes.RO_Node_Type;
      I : Nodes.Index_Type;
   begin
      Search_Minimum_Node (Tree, N, I, State);
      if State = Success then
         Key   := Nodes.Key (N, I);
         Value := Nodes.Value (N, I);
      end if;
   end Search_Minimum;

end Searches;

