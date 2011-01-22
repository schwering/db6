-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Types.Gen_Reals is

   function Compare
     (Left, Right : Real_Type)
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


   function New_Context return Context_Type is
   begin
      return (null record);
   end New_Context;


   function New_Read_Context return Read_Context_Type
   renames New_Context;


   function New_Write_Context return Write_Context_Type
   renames New_Context;


   function Size_Bound (Real : Real_Type)
      return Blocks.Size_Type
   is
      function Size_Of is new Blocks.Size_Of (Real_Type);
   begin
      return Size_Of (Real);
   end Size_Bound;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Real    : in     Real_Type)
   is
      pragma Unreferenced (Context);
      procedure Write is new Blocks.Write (Real_Type);
   begin
      Write (Block, Cursor, Real);
   end Write;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Real    :    out Real_Type)
   is
      pragma Unreferenced (Context);
      procedure Read is new Blocks.Read (Real_Type);
   begin
      Read (Block, Cursor, Real);
   end Read;


   procedure Skip
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type)
   is
      Real : Real_Type;
   begin
      Read (Context, Block, Cursor, Real);
   end Skip;


   function Image (R : Real_Type) return String
   is
      S : constant String := Real_Type'Image (R);
   begin
      for I in S'Range loop
         if S (I) = ' ' then
            return S (I .. S'Last);
         end if;
      end loop;
      return S;
   end Image;

end DB.Types.Gen_Reals;

