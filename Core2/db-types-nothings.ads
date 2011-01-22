-- Abstract:
--
-- Empty value.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Blocks;
with DB.Utils;

package DB.Types.Nothings is
   pragma Pure;

   type Nothing_Type is null record;

   type Read_Context_Type is null record;
   type Write_Context_Type is null record;

   Is_Context_Free_Serialization : constant Boolean := True;

   function Compare
     (Left, Right : Nothing_Type)
      return Utils.Comparison_Result_Type;

   function New_Read_Context return Read_Context_Type;

   function New_Write_Context return Write_Context_Type;

   function Size_Bound (Nothing : Nothing_Type) return Blocks.Size_Type;

   procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Nothing : in     Nothing_Type)
   is null;

   procedure Read
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Nothing :    out Nothing_Type)
   is null;

   procedure Skip
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type)
   is null;

   function Image (Nothing : Nothing_Type) return String;

private
   type Context_Type is null record;

   pragma Inline (New_Read_Context);
   pragma Inline (New_Write_Context);
   pragma Inline (Size_Bound);
   pragma Inline (Write);
   pragma Inline (Read);
   pragma Inline (Skip);

end DB.Types.Nothings;

