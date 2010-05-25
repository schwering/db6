-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks;

separate (DB.Types.Gen_Strings.Gen_Bounded)
package body Deflate is

   overriding procedure Initialize (Context : in out Context_Type) is
   begin
      null;
   end Initialize;

   overriding procedure Adjust (Context : in out Context_Type) is
   begin
      null;
   end Adjust;

   overriding procedure Finalize (Context : in out Context_Type) is
   begin
      null;
   end Finalize;


   function New_Context
      return Context_Type
   is
      C : Context_Type;
   begin
      return C;
   end New_Context;


   function Size_Bound
     (S : String_Type)
      return Blocks.Size_Type
   is
      function Size_Of_String is
         new Blocks.Size_Of_Array(Index_Type, Item_Type, Buffer_Type);
   begin
      return Size_Of_String(S.Buffer, 1, S.Length);
   end Size_Bound;


   procedure Write
     (Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      S       : in     String_Type)
   is
      procedure Write_String is
         new Blocks.Write_Array(Index_Type, Item_Type, Buffer_Type);
   begin
      Write_String(Block, Cursor, S.Buffer, 1, S.Length);
   end Write;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      S       : in     String_Type)
   is
      pragma Unreferenced (Context);
   begin
      Write(Block, Cursor, S);
   end Write;


   procedure Read
     (Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      S       :    out String_Type)
   is
      procedure Read_String is
         new Blocks.Read_Array(Index_Type, Item_Type, Buffer_Type);
   begin
      Read_String(Block, Cursor, S.Buffer, 1, S.Length);
   end Read;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      S       :    out String_Type)
   is
      pragma Unreferenced (Context);
   begin
      Read(Block, Cursor, S);
   end Read;


   procedure Skip
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type)
   is
      S : String_Type;
   begin
      Read(Context, Block, Cursor, S);
   end Skip;

end Deflate;

