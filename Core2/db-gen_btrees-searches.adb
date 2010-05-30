-- Abstract:
--
-- The search descends to a leaf and then moves right as long as there is hope
-- to find the key.
-- During all this time, nothing is locked.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.Gen_BTrees)
package body Searches is

   procedure Search
     (Tree     : in out Tree_Type;
      Key      : in     Keys.Key_Type;
      Value    :    out Values.Value_Type;
      State    :    out State_Type)
   is
      procedure Find_Leaf (N : out Nodes.Node_Type)
      is
         N_A : Nodes.Valid_Address_Type := Root_Address;
      begin
         loop
            Read_Node(Tree, N_A, N);
            exit when Nodes.Is_Leaf(N);
            N_A := Scan_Node(N, Key);
         end loop;
      end Find_Leaf;

      pragma Assert (Tree.Initialized);
      N : Nodes.RO_Node_Type;
   begin
      Find_Leaf(N);
      loop
         pragma Assert (Nodes.Is_Leaf(N));
         declare
            use type Utils.Comparison_Result_Type;
            I : constant Nodes.Index_Type := Nodes.Key_Position(N, Key);
         begin
            if Nodes.Is_Valid(I) then
               case Keys.Compare(Key, Nodes.Key(N, I)) is
                  when Utils.Equal =>
                     Value := Nodes.Value(N, I);
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
   end Search;


   procedure Minimum
     (Tree     : in out Tree_Type;
      Key      :    out Keys.Key_Type;
      Value    :    out Values.Value_Type;
      State    :    out State_Type)
   is
      pragma Assert (Tree.Initialized);
      use type Nodes.Degree_Type;
      N_A : Nodes.Valid_Address_Type := Root_Address;
      N   : Nodes.RO_Node_Type;
   begin
      loop
         Read_Node(Tree, N_A, N);
         if Nodes.Degree(N) = 0 then
            State := Failure;
            return;
         end if;
         if Nodes.Is_Leaf(N) then
            Key   := Nodes.Key(N, 1);
            Value := Nodes.Value(N, 1);
            State := Success;
            return;
         end if;
         N_A := Nodes.Child(N, 1);
      end loop;
   end Minimum;

end Searches;

