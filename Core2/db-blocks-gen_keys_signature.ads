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

with DB.Utils;

generic

   pragma Warnings (Off); -- Disable `unreferenced' warnings

   type Key_Type is private;
   type Context_Type is private;

   with function Compare
     (A, B : Key_Type)
      return Utils.Comparison_Result_Type is <>;

   with function New_Context
      return Context_Type is <>;

   with function Size_Bound
     (Key : Key_Type)
      return Blocks.Size_Type is <>;

   with procedure Read
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Key     :    out Key_Type) is <>;

   with procedure Skip
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type) is <>;

   with procedure Write
     (Context : in out Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Key     : in     Key_Type) is <>;

   pragma Warnings (On);

package DB.Blocks.Gen_Keys_Signature is
   pragma Pure;

private
   function "=" (A, B : Key_Type) return Boolean;

end DB.Blocks.Gen_Keys_Signature;

