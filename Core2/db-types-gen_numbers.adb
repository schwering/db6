-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Types.Gen_Numbers is

   function Compare
     (Left, Right : Number_Type)
      return Utils.Comparison_Result_Type is
   begin
      if Left < Right then
         return Utils.Less;
      elsif Left = Right then
         return Utils.Equal;
      else
         return Utils.Greater;
      end if;
   end Compare;


   function New_Context
      return Context_Type is
   begin
      return (null record);
   end New_Context;


   function New_Read_Context
      return Read_Context_Type
   renames New_Context;


   function New_Write_Context
      return Write_Context_Type
   renames New_Context;


   function Size_Bound
     (Number : Number_Type)
      return Blocks.Size_Type
   is
      function Size_Of is new Blocks.Size_Of(Number_Type);
   begin
      return Size_Of(Number);
   end Size_Bound;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Number  : in     Number_Type)
   is
      pragma Unreferenced (Context);
      procedure Write is new Blocks.Write(Number_Type);
   begin
      Write(Block, Cursor, Number);
   end Write;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Number  :    out Number_Type)
   is
      pragma Unreferenced (Context);
      procedure Read is new Blocks.Read(Number_Type);
   begin
      Read(Block, Cursor, Number);
   end Read;


   procedure Skip
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type)
   is
      Number : Number_Type;
   begin
      Read(Context, Block, Cursor, Number);
   end Skip;


   function Short_Bound
     (Left  : Number_Type)
      return Number_Type is
   begin
      return Left;
   end Short_Bound;


   function Short_Delimiter
     (Left  : Number_Type;
      Right : Number_Type)
      return Number_Type
   is
      pragma Unreferenced (Right);
   begin
      return Left;
   end Short_Delimiter;

end DB.Types.Gen_Numbers;

