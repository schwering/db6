-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.Gen_BTrees)
package body Retrieval is

   procedure Search
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type)
   is
      procedure Find_Leaf_Address
        (N_A : out Nodes.Valid_Address_Type) is
      begin
         N_A := Root_Address;
         loop
            declare
               N : Nodes.RO_Node_Type;
            begin
               Read_Node(Tree, N_A, N);
               exit when Nodes.Is_Leaf(N);
               N_A := Scan_Node(N, Key);
            end;
         end loop;
      end Find_Leaf_Address;

      pragma Assert (Tree.Initialized);
      N_A : Nodes.Valid_Address_Type;
      N   : Nodes.RO_Node_Type;
      I   : Nodes.Index_Type;
   begin
      Find_Leaf_Address(N_A);
      loop
         Read_Node(Tree, N_A, N);
         I := Nodes.Key_Position(N, Key);
         if Nodes.Is_Valid(I) then
            if Nodes.Key(N, I) = Key then
               Value := Nodes.Value(N, I);
               State := Success;
               return;
            else
               State := Failure;
               return;
            end if;
         end if;
         if not Nodes.Is_Valid(Nodes.Link(N)) then
            State := Failure;
            return;
         end if;
         N_A := Nodes.Valid_Link(N);
      end loop;

   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Search;


   procedure Minimum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type)
   is
      pragma Assert (Tree.Initialized);
      N_A : Nodes.Valid_Address_Type := Root_Address;
   begin
      loop
         declare
            use type Nodes.Degree_Type;
            N : Nodes.RO_Node_Type;
         begin
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
         end;
      end loop;

   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Minimum;

end Retrieval;

