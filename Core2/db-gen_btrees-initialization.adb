-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.Gen_BTrees)
package body Initialization is

   procedure Create
     (ID : in String)
   is
      File : Block_IO.File_Type;
      Root : constant Nodes.RW_Node_Type :=
         Nodes.Root_Node(Is_Leaf => True, Level => Nodes.Leaf_Level);
   begin
      Block_IO.Create(ID, File);
      declare
      begin
         Block_IO.Write(File, Block_IO.Valid_Address_Type(Root_Address),
                        Nodes.To_Block(Root));
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

