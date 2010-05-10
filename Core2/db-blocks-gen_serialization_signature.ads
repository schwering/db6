-- Abstract:
--
-- Block serialization signature package.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic

   pragma Warnings (Off); -- Disable `unreferenced' warnings

   type Object_Type is private;
   type Context_Type is private;

   with function New_Context
      return Context_Type;

   with function Size_Bound
     (Object : Object_Type)
      return Blocks.Size_Type;

   with procedure Read
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Object  :    out Object_Type);

   with procedure Skip
     (Context : in out Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type);

   with procedure Write
     (Context : in out Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Object  : in     Object_Type);

   pragma Warnings (On);

package DB.Blocks.Gen_Serialization_Signature is
   pragma Pure;
end DB.Blocks.Gen_Serialization_Signature;

