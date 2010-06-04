-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

separate (DB.Gen_BTrees)
package body Initialization is

   procedure Prepare_File
     (File : in out Block_IO.File_Type)
   is
      Root : constant Nodes.RW_Node_Type :=
         Nodes.Root_Node(Nodes.Leaf_Level);
   begin
      Block_IO.Write(File, Block_IO.Valid_Address_Type(Root_Address),
                     Nodes.To_Block(Root));
   exception
      when others =>
         Block_IO.Close(File);
         raise;
   end Prepare_File;


   procedure Create
     (Tree : in out Tree_Type;
      ID   : in     String)
   is
      pragma Precondition (not Tree.Initialized);
      pragma Precondition (not Tree.Finalized);
   begin
      Tree.Initialized := False;
      Block_IO.Create(ID, Tree.File);
      Prepare_File(Tree.File);
      Tree.Initialized := True;
   end Create;


   procedure Create_Temporary
     (Tree : in out Tree_Type;
      ID   : in     String)
   is
      pragma Precondition (not Tree.Initialized);
      pragma Precondition (not Tree.Finalized);
   begin
      Tree.Initialized := False;
      Block_IO.Create_And_Open_Temporary(ID, Tree.File);
      Prepare_File(Tree.File);
      Tree.Initialized := True;
   end Create_Temporary;


   procedure Open
     (Tree : in out Tree_Type;
      ID   : in     String)
   is
      pragma Precondition (not Tree.Initialized);
      pragma Precondition (not Tree.Finalized);
   begin
      Tree.Initialized := False;
      Block_IO.Open(ID, Tree.File);
      Tree.Initialized := True;
   end Open;


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

