-- Abstract:
--
-- Value type.
--
-- Copyright 2008, 2009 Christoph Schwering

with DB.IO.Blocks;

with System.Storage_Elements;

package DB.Types.Values is
   pragma Pure;

   type Value_Type is mod 2**64;

   type Context_Type is private;

   Is_Context_Free_Serialization : constant Boolean := True;

   function Size_Of
     (Value : Value_Type)
      return IO.Blocks.Size_Type;

   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Value   : in     Value_Type);

   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Value   :    out Value_Type);

   procedure Skip
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type);

   function To_Storage_Array
     (Value : Value_Type)
      return System.Storage_Elements.Storage_Array;

   function From_Storage_Array
     (Arr : System.Storage_Elements.Storage_Array)
      return Value_Type;

private
   type Context_Type is null record;

   pragma Inline (Size_Of);
   pragma Inline (Write);
   pragma Inline (Read);
   pragma Inline (Skip);

end DB.Types.Values;

