-- Abstract:
--
-- Generic number type.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.IO.Blocks;
with DB.Utils;

generic
   type P_Number_Type is (<>);
package DB.Types.Gen_Numbers is
   pragma Pure;

   subtype Number_Type is P_Number_Type;

   type Context_Type is private;

   Is_Context_Free_Serialization : constant Boolean := True;

   function Compare
     (Left, Right : Number_Type)
      return Utils.Comparison_Result_Type;

   function New_Context
      return Context_Type;

   function Size_Bound
     (Number : Number_Type)
      return IO.Blocks.Size_Type;

   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Number  : in     Number_Type);

   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Number  :    out Number_Type);

   procedure Skip
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type);

   function Short_Bound
     (Left  : Number_Type)
      return Number_Type;

   function Short_Delimiter
     (Left  : Number_Type;
      Right : Number_Type)
      return Number_Type;

private
   type Context_Type is null record;

   pragma Inline (New_Context);
   pragma Inline (Size_Bound);
   pragma Inline (Write);
   pragma Inline (Read);
   pragma Inline (Skip);

end DB.Types.Gen_Numbers;

