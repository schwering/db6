-- Abstract:
--
-- Generic real type.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Blocks;
with DB.Utils;

generic
   type P_Real_Type is digits <>;
package DB.Types.Gen_Reals is
   pragma Pure;

   subtype Real_Type is P_Real_Type;

   type Context_Type is private;
   subtype Read_Context_Type is Context_Type;
   subtype Write_Context_Type is Context_Type;

   Is_Context_Free_Serialization : constant Boolean := True;

   function Compare
     (Left, Right : Real_Type)
      return Utils.Comparison_Result_Type;

   function New_Read_Context return Read_Context_Type;

   function New_Write_Context return Write_Context_Type;

   function Size_Bound (Real : Real_Type) return Blocks.Size_Type;

   procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Real    : in     Real_Type);

   procedure Read
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Real    :    out Real_Type);

   procedure Skip
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type);

   function Image (R : Real_Type) return String;

private
   type Context_Type is null record;

   pragma Inline (New_Read_Context);
   pragma Inline (New_Write_Context);
   pragma Inline (Size_Bound);
   pragma Inline (Write);
   pragma Inline (Read);
   pragma Inline (Skip);

end DB.Types.Gen_Reals;

