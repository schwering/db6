-- Abstract:
--
-- Generic Discrete type.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Blocks;
with DB.Utils;

generic
   type P_Discrete_Type is (<>);
package DB.Types.Gen_Discretes is
   pragma Pure;

   subtype Discrete_Type is P_Discrete_Type;

   type Context_Type is private;
   subtype Read_Context_Type is Context_Type;
   subtype Write_Context_Type is Context_Type;

   Is_Context_Free_Serialization : constant Boolean := True;

   function Compare
     (Left, Right : Discrete_Type)
      return Utils.Comparison_Result_Type;

   function New_Read_Context return Read_Context_Type;

   function New_Write_Context return Write_Context_Type;

   function Size_Bound (Discrete : Discrete_Type) return Blocks.Size_Type;

   procedure Write
     (Context   : in out Write_Context_Type;
      Block     : in out Blocks.Base_Block_Type;
      Cursor    : in out Blocks.Cursor_Type;
      Discrete  : in     Discrete_Type);

   procedure Read
     (Context   : in out Context_Type;
      Block     : in     Blocks.Base_Block_Type;
      Cursor    : in out Blocks.Cursor_Type;
      Discrete  :    out Discrete_Type);

   procedure Skip
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type);

   function Image (D : Discrete_Type) return String;

private
   type Context_Type is null record;

   pragma Inline (New_Read_Context);
   pragma Inline (New_Write_Context);
   pragma Inline (Size_Bound);
   pragma Inline (Write);
   pragma Inline (Read);
   pragma Inline (Skip);

end DB.Types.Gen_Discretes;

