-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.IO.Blocks;

procedure DB.Gen_BTrees.Gen_Stats
  (Tree                   : in out Tree_Type;
   Height                 :    out Natural;
   Blocks                 :    out Natural;
   Free_Blocks            :    out Natural;
   Max_Degree             :    out Natural;
   Avg_Degree             :    out Natural;
   Min_Degree             :    out Natural;
   Bytes_Wasted_In_Blocks :    out Long_Integer;
   Bytes_In_Blocks        :    out Long_Integer)
is
   Transaction : RO_Transaction_Type := New_RO_Transaction(Tree);

   procedure Read_Node
     (Tree        : in out Tree_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           :    out Nodes.Node_Type) is
   begin
      Read_Node(Tree, Transaction, N_A, N);
   end Read_Node;

   A : Nodes.Valid_Address_Type;
   Degree_Sum : Long_Integer := 0;
begin -- Gen_Traverse
   Start_Transaction(Tree, Transaction);
   Height                 := 0;
   Get_Height(Tree, Height);
   Blocks                 := 0;
   Free_Blocks            := 0;
   Max_Degree             := 0;
   Min_Degree             := Natural(Nodes.Degree_Type'Last);
   Bytes_Wasted_In_Blocks := 0;
   Bytes_In_Blocks        := 0;
   A := Nodes.Valid_Address_Type(Block_IO.Succ(Block_IO.First));
   loop
      declare
         use type Nodes.Degree_Type;
         use type IO.Blocks.Size_Type;
         N : Nodes.RO_Node_Type;
         Degree : Nodes.Degree_Type;
      begin
         Read_Node(Tree, A, N);
         Blocks := Blocks + 1;
         if Nodes.Is_Free(N) then
            Free_Blocks := Free_Blocks + 1;
         else
            Degree := Nodes.Degree(N);
            Degree_Sum := Degree_Sum + Long_Integer(Degree);
            if Degree > Nodes.Degree_Type(Max_Degree) then
               Max_Degree := Natural(Degree);
            end if;
            if Degree < Nodes.Degree_Type(Min_Degree) then
               Min_Degree := Natural(Degree);
            end if;
            if not Nodes.Is_Root(N) then
               Bytes_In_Blocks := Bytes_In_Blocks
                  + Long_Integer(DB.IO.Blocks.Block_Size);
               Bytes_Wasted_In_Blocks := Bytes_Wasted_In_Blocks
                  + Long_Integer(DB.IO.Blocks.Block_Size - Nodes.Size_Of(N));
            end if;
         end if;
         A := Nodes.Valid_Address_Type(Block_IO.Succ
                                          (Block_IO.Valid_Address_Type(A)));
      exception
         when IO_Error =>
            exit;
      end;
   end loop;
   Avg_Degree := Natural(Degree_Sum / Long_Integer(Blocks));
   Finish_Transaction(Tree, Transaction);
exception
   when others =>
      Finish_Transaction(Tree, Transaction);
      raise;
end DB.Gen_BTrees.Gen_Stats;

