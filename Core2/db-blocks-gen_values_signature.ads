-- Abstract:
--
-- Key serialization signature package.
-- A key is the same like a value but has a comparison function.
--
-- Design Notes:
--
-- The main reason that we distinguish between Keys and Values and this point is
-- that this allows for types Key_Type and Value_Type instead of some ugly
-- generic Object_Type or so.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic

   pragma Warnings (Off); -- Disable `unreferenced' warnings

   type Value_Type is private;
   type Read_Context_Type is private;
   type Write_Context_Type is private;

   with function New_Read_Context return Read_Context_Type is <>;

   with function New_Write_Context return Write_Context_Type is <>;

   with function Size_Bound (Value : Value_Type) return Blocks.Size_Type is <>;

   with procedure Read
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Value   :    out Value_Type) is <>;

   with procedure Skip
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type) is <>;

   with procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Value   : in     Value_Type) is <>;

   pragma Warnings (On);

package DB.Blocks.Gen_Values_Signature is
   pragma Pure;
end DB.Blocks.Gen_Values_Signature;

