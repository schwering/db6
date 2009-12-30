with Ada.Text_IO; use Ada.Text_IO;
with This_Computer;
with DB.IO.Blocks;
with DB.IO.Blocks.CFS_IO;

procedure Mkfs
is
   use type DB.IO.Blocks.Size_Type;
begin
   Put_Line("Making FS "& This_Computer.Device_Name);
   DB.IO.Blocks.CFS_IO.Make_Filesystem(This_Computer.Device_Name, 2**29);
end Mkfs;

