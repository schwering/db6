-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.Gen_BTrees)
package body Misc is

   procedure Count
     (Tree  : in out Tree_Type;
      Count :    out Count_Type)
   is
      pragma Assert (Tree.Initialized);

      N_A : Nodes.Valid_Address_Type :=
         Nodes.Valid_Address_Type(Block_IO.First);
   begin
      Count := 0;
      loop
         declare
            use type Nodes.Degree_Type;
            N : Nodes.RO_Node_Type;
         begin
            Read_Node(Tree, N_A, N);
            if Nodes.Is_Leaf(N) then
               Count := Count + Count_Type(Nodes.Degree(N));
            end if;
            N_A := Nodes.Valid_Address_Type(Block_IO.Succ
                        (Block_IO.Valid_Address_Type(N_A)));
         exception
            when IO_Error =>
               exit;
         end;
      end loop;
   end Count;


--   procedure Count
--     (Tree  : in out Tree_Type;
--      Count :    out Count_Type)
--   is
--      pragma Assert (Tree.Initialized);
--
--      N_A : Nodes.Valid_Address_Type := Root_Address;
--   begin
--      loop
--         declare
--            N : Nodes.RO_Node_Type;
--         begin
--            Read_Node(Tree, N_A, N);
--            exit when Nodes.Is_Leaf(N);
--            N_A := Nodes.Child(N, 1);
--         end;
--      end loop;
--      Count := 0;
--      loop
--         declare
--            N : Nodes.RO_Node_Type;
--         begin
--            Read_Node(Tree, N_A, N);
--            Count := Count + Count_Type(Nodes.Degree(N));
--            exit when not Nodes.Is_Valid(Nodes.Link(N));
--            N_A := Nodes.Valid_Link(N);
--         end;
--      end loop;
--   end Count;


   procedure Reorganize
     (Tree  : in out Tree_Type;
      State :    out State_Type) is
   begin
      null;
   end Reorganize;

end Misc;

