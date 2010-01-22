-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.IO.Blocks;

separate (DB.Types.Gen_Strings.Gen_Bounded)
package body Uncompressed is

   function New_Context
      return Context_Type is
   begin
      return (null record);
   end New_Context;


   function Size_Bound
     (S : String_Type)
      return IO.Blocks.Size_Type
   is
      function Size_Of_String is
         new IO.Blocks.Size_Of_Array(Index_Type, Item_Type, Buffer_Type);
   begin
      return Size_Of_String(S.Buffer, 1, S.Length);
   end Size_Bound;


   procedure Write
     (Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       : in     String_Type)
   is
      procedure Write_String is
         new IO.Blocks.Write_Array(Index_Type, Item_Type, Buffer_Type);
   begin
      Write_String(Block, Cursor, S.Buffer, 1, S.Length);
   end Write;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       : in     String_Type)
   is
      pragma Unreferenced (Context);
   begin
      Write(Block, Cursor, S);
   end Write;


   procedure Read
     (Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       :    out String_Type)
   is
      procedure Read_String is
         new IO.Blocks.Read_Array(Index_Type, Item_Type, Buffer_Type);
   begin
      Read_String(Block, Cursor, S.Buffer, 1, S.Length);
   end Read;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      S       :    out String_Type)
   is
      pragma Unreferenced (Context);
   begin
      Read(Block, Cursor, S);
   end Read;


   procedure Skip
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type)
   is
      S : String_Type;
   begin
      Read(Context, Block, Cursor, S);
   end Skip;

end Uncompressed;

