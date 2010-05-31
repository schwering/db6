-- Abstract:
--
-- The search descends to a leaf and then moves right as long as there is hope
-- to find the key.
-- During all this time, nothing is locked.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.Gen_BTrees)
package body Searches is

   procedure Search_Node
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      N     :    out Nodes.Node_Type;
      Index :    out Nodes.Valid_Index_Type;
      State :    out State_Type)
   is
      procedure Find_Leaf
      is
         pragma Inline (Find_Leaf);
         N_A : Nodes.Valid_Address_Type := Root_Address;
      begin
         loop
            Read_Node(Tree, N_A, N);
            exit when Nodes.Is_Leaf(N);
            N_A := Scan_Node(N, Key);
         end loop;
      end Find_Leaf;

      pragma Assert (Tree.Initialized);
   begin
      Find_Leaf;
      loop
         pragma Assert (Nodes.Is_Leaf(N));
         declare
            use type Utils.Comparison_Result_Type;
            I : constant Nodes.Index_Type := Nodes.Key_Position(N, Key);
         begin
            if Nodes.Is_Valid(I) then
               case Keys.Compare(Key, Nodes.Key(N, I)) is
                  when Utils.Equal =>
                     Index := I;
                     State := Success;
                     return;
                  when Utils.Less =>
                     State := Failure;
                     return;
                  when Utils.Greater =>
                     raise Tree_Error;
               end case;
            end if;
         end;
         if not Nodes.Is_Valid(Nodes.Link(N)) then
            State := Failure;
            return;
         end if;
         declare
            High_Key     : Keys.Key_Type;
            Has_High_Key : Boolean;
         begin
            Nodes.Get_High_Key(N, High_Key, Has_High_Key);
            if Has_High_Key and then Key < High_Key then
               State := Failure;
               return;
            end if;
         end;
         Read_Node(Tree, Nodes.Valid_Link(N), N);
      end loop;
   end Search_Node;


   procedure Search
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type)
   is
      N : Nodes.RO_Node_Type;
      I : Nodes.Valid_Index_Type;
   begin
      Search_Node(Tree, Key, N, I, State);
      if State = Success then
         Value := Nodes.Value(N, I);
      end if;
   end Search;


   procedure Search_Minimum_Node
     (Tree  : in out Tree_Type;
      N     :    out Nodes.Node_Type;
      Index :    out Nodes.Valid_Index_Type;
      State :    out State_Type)
   is
      pragma Assert (Tree.Initialized);
      use type Nodes.Degree_Type;
      N_A : Nodes.Valid_Address_Type := Root_Address;
   begin
      loop
         Read_Node(Tree, N_A, N);
         if Nodes.Is_Inner(N) then
            N_A := Nodes.Child(N, 1);
         else
            if Nodes.Degree(N) > 0 then
               Index := 1;
               State := Success;
            elsif Nodes.Is_Valid(Nodes.Link(N)) then
               N_A := Nodes.Valid_Link(N);
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
      I : Nodes.Valid_Index_Type;
   begin
      Search_Minimum_Node(Tree, N, I, State);
      if State = Success then
         Key   := Nodes.Key(N, I);
         Value := Nodes.Value(N, I);
      end if;
   end Search_Minimum;


end Searches;

