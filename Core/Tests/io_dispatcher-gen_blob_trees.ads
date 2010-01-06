with DB.Gen_Blob_Trees;

private
generic
   with package Blob_Trees is new DB.Gen_Blob_Trees (<>);
   with procedure Check (T : in out Blob_Trees.Tree_Type);
procedure IO_Dispatcher.Gen_Blob_Trees;

