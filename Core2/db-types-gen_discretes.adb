-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Types.Gen_Discretes is

   function Compare
     (Left, Right : Discrete_Type)
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


   function Size_Bound (Discrete : Discrete_Type)
      return Blocks.Size_Type
   is
      function Size_Of is new Blocks.Size_Of (Discrete_Type);
   begin
      return Size_Of (Discrete);
   end Size_Bound;


   procedure Write
     (Context   : in out Context_Type;
      Block     : in out Blocks.Base_Block_Type;
      Cursor    : in out Blocks.Cursor_Type;
      Discrete  : in     Discrete_Type)
   is
      pragma Unreferenced (Context);
      procedure Write is new Blocks.Write (Discrete_Type);
   begin
      Write (Block, Cursor, Discrete);
   end Write;


   procedure Read
     (Context   : in out Context_Type;
      Block     : in     Blocks.Base_Block_Type;
      Cursor    : in out Blocks.Cursor_Type;
      Discrete  :    out Discrete_Type)
   is
      pragma Unreferenced (Context);
      procedure Read is new Blocks.Read (Discrete_Type);
   begin
      Read (Block, Cursor, Discrete);
   end Read;


   procedure Skip
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type)
   is
      Discrete : Discrete_Type;
   begin
      Read (Context, Block, Cursor, Discrete);
   end Skip;


   function Image (D : Discrete_Type) return String
   is
      S : constant String := Discrete_Type'Image (D);
   begin
      for I in S'Range loop
         if S (I) = ' ' then
            return S (I .. S'Last);
         end if;
      end loop;
      return S;
   end Image;

end DB.Types.Gen_Discretes;

