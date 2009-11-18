-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

with DB.IO.Blocks;

separate (DB.Gen_BTrees)
package body Initialization is

   procedure Create
     (ID : in String)
   is
      File : Block_IO.File_Type;
      Root : constant Nodes.Node_Type := Nodes.Root_Node(Is_Leaf => True);
      Free : constant Nodes.Node_Type := Nodes.Free_Node;
   begin
      Block_IO.Create(ID, File);
      declare
      begin
         Block_IO.Write(File, Block_IO.Valid_Address_Type(Root_Address),
                        Nodes.To_Block(Root));
         Block_IO.Write(File, Block_IO.Valid_Address_Type(Free_Address),
                        Nodes.To_Block(Free));
         Block_IO.Close(File);
      exception
         when others =>
            Block_IO.Close(File);
            raise;
      end;
   end Create;


   procedure Initialize
     (Tree : out Tree_Type;
      ID   : in  String)
   is
      pragma Assert (not Tree.Initialized);
      pragma Assert (not Tree.Finalized);
   begin
      Block_IO.Open(ID, Tree.File);

      if Block_IO.Needs_Explicit_Block_Count then
         declare
            procedure Increment
              (Count : in out Block_IO.Valid_Address_Type;
               Init  : in out Boolean) is
            begin
               if not Init then
                  Count := Block_IO.First;
                  Init  := True;
               else
                  Count := Block_IO.Succ(Count);
               end if;
            end Increment;

            procedure Visit_Nodes
              (Count : in out Block_IO.Valid_Address_Type;
               Init  : in out Boolean)
            is
               F_A : Nodes.Valid_Address_Type := Root_Address;
               N_A : Nodes.Valid_Address_Type := Root_Address;
            begin
               loop
                  Increment(Count, Init);
                  declare
                     N : Nodes.Node_Type;
                  begin
                     Read_Node(Tree, N_A, N);
                     if Nodes.Is_Valid(Nodes.Right_Neighbor(N)) then
                        N_A := Nodes.To_Valid_Address(Nodes.Right_Neighbor(N));
                     elsif Nodes.Is_Inner(N) then
                        declare
                           F : Nodes.Node_Type;
                        begin
                           Read_Node(Tree, F_A, F);
                           F_A := Nodes.Child(F, 1);
                           N_A := F_A;
                        end;
                     else
                        exit;
                     end if;
                  end;
               end loop;
            end Visit_Nodes;

            procedure Visit_Free
              (Count : in out Block_IO.Valid_Address_Type;
               Init  : in out Boolean)
            is
               N_A : Nodes.Address_Type := Nodes.To_Address(Free_Address);
            begin
               while Nodes.Is_Valid(N_A) loop
                  Increment(Count, Init);
                  declare
                     N : Nodes.Node_Type;
                  begin
                     Read_Node(Tree, Nodes.To_Valid_Address(N_A), N);
                     N_A := Nodes.Right_Neighbor(N);
                  end;
               end loop;
            end Visit_Free;

            Init  : Boolean := False;
            Count : Block_IO.Valid_Address_Type;
         begin
            Visit_Nodes(Count, Init);
            Visit_Free(Count, Init);
            Block_IO.Set_Block_Count(Tree.File, Block_IO.To_Address(Count));
         exception
            when others =>
               Block_IO.Close(Tree.File);
               raise;
         end;
      end if;

      Tree.Initialized := True;
   end Initialize;


   procedure Finalize
     (Tree : in out Tree_Type) is
   begin
      if Tree.Initialized then
         Block_IO.Close(Tree.File);
         Tree.Initialized := False;
      end if;
      Tree.Finalized := True;
   end Finalize;

end Initialization;

