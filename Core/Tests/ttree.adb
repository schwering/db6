with Gen_TTree;

with DB.IO.Blocks.Memory_IO;

procedure TTree is
   procedure Wrapped_TTree is new Gen_TTree(DB.IO.Blocks.Memory_IO.IO);
begin
   Wrapped_TTree;
end;

