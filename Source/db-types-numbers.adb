package body DB.Types.Numbers is

   procedure Get_Size_Of
     (Context : in out Context_Type;
      Number  : in     Number_Type;
      Size    :    out IO.Blocks.Size_Type)
   is
      pragma Unreferenced (Context);
      function Size_Of is new IO.Blocks.Size_Of(Number_Type);
   begin
      Size := Size_Of(Number);
   end Get_Size_Of;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Number  : in     Number_Type)
   is
      pragma Unreferenced (Context);
      procedure Write is new IO.Blocks.Write(Number_Type);
   begin
      Write(Block, Cursor, Number);
   end Write;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Number  :    out Number_Type)
   is
      pragma Unreferenced (Context);
      procedure Read is new IO.Blocks.Read(Number_Type);
   begin
      Read(Block, Cursor, Number);
   end Read;


   procedure Skip
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type)
   is
      Number : Number_Type;
   begin
      Read(Context, Block, Cursor, Number);
   end Skip;

end DB.Types.Numbers;

