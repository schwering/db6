-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks;

separate (DB.Types.Gen_Strings.Gen_Unbounded)
package body Uncompressed is

   function New_Context
      return Context_Type is
   begin
      return Context_Type'(others => <>);
   end New_Context;


   function Size_Bound
     (S : String_Type)
      return Blocks.Size_Type
   is
      type Definite_Buffer_Type is
         array (Index_Type range S.S.Buffer'Range) of Item_Type;
      function Size_Of_Length is new Blocks.Size_Of(Length_Type);
      function Size_Of_Buffer is new Blocks.Size_Of(Definite_Buffer_Type);
      use type Blocks.Size_Type;
   begin
      return Size_Of_Length(S.S.Buffer'Length)
           + Size_Of_Buffer(Definite_Buffer_Type(S.S.Buffer));
   end Size_Bound;


   procedure Write
     (Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      S       : in     String_Type)
   is
      type Definite_Buffer_Type is
         array (Index_Type range S.S.Buffer'Range) of Item_Type;
      procedure Write_Length is new Blocks.Write(Length_Type);
      procedure Write_Buffer is new Blocks.Write(Definite_Buffer_Type);
   begin
      Write_Length(Block, Cursor, S.S.Buffer'Length);
      Write_Buffer(Block, Cursor, Definite_Buffer_Type(S.S.Buffer));
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
      procedure Read_Length is new Blocks.Read(Length_Type);
      Length : Length_Type;
   begin
      Read_Length(Block, Cursor, Length);
      declare
         type Definite_Buffer_Type is
            array (Index_Type range 1 .. Length) of Item_Type;
         procedure Read_Buffer is
            new Blocks.Read(Definite_Buffer_Type);
         Definite_Buffer : Definite_Buffer_Type;
      begin
         Read_Buffer(Block, Cursor, Definite_Buffer);
         S := New_String(Indefinite_Buffer_Type(Definite_Buffer));
      end;
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

end Uncompressed;

