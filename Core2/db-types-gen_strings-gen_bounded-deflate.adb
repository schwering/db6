-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;
with DB.Blocks;

separate (DB.Types.Gen_Strings.Gen_Bounded)
package body Deflate is

   overriding
   procedure Initialize (Context : in out Context_Type) is
   begin
      Context.Read      := null;
      Context.Write     := null;
      Context.Ref_Count := new Natural'(1);
   end Initialize;


   overriding
   procedure Adjust (Context : in out Context_Type) is
   begin
      Context.Ref_Count.all := Context.Ref_Count.all + 1;
   end Adjust;


   overriding
   procedure Finalize (Context : in out Context_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Natural, Natural_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (Zlib.Filter_Type, Zlib_Ref_Type);
   begin
      Context.Ref_Count.all := Context.Ref_Count.all - 1;
      if Context.Ref_Count.all = 0 then
         Free(Context.Ref_Count);
         if Context.Read /= null then
            Free(Context.Read);
         end if;
         if Context.Write /= null then
            Free(Context.Write);
         end if;
      end if;
   end Finalize;


   function New_Context
      return Context_Type is
   begin
      return Context_Type'(Ada.Finalization.Controlled with others => <>);
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
     (Context : in out Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      S       : in     String_Type)
   is
      procedure Write_String is
         new Blocks.Write_Array(Index_Type, Item_Type, Buffer_Type);
   begin
      if Context.Write = null then
         Context.Write := new Zlib.Filter_Type;
         Zlib.Inflate_Init(Context.Write.all);
      end if;
      Write_String(Block, Cursor, S.Buffer, 1, S.Length);
   end Write;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      S       :    out String_Type)
   is
      procedure Read_String is
         new Blocks.Read_Array(Index_Type, Item_Type, Buffer_Type);
   begin
      if Context.Read = null then
         Context.Read := new Zlib.Filter_Type;
         Zlib.Deflate_Init(Context.Read.all);
      end if;
      Read_String(Block, Cursor, S.Buffer, 1, S.Length);
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

