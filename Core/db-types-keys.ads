-- Abstract:
--
-- Key type. A key consists of a row, a column and a timestamp.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.IO.Blocks;
pragma Warnings (Off);
with DB.Types.Strings.Bounded;
with DB.Types.Strings.Unbounded;
pragma Warnings (On);
with DB.Types.Times;
with DB.Utils;

package DB.Types.Keys is
   pragma Elaborate_Body;

   package Rows    renames Strings.Bounded;
   package Columns renames Strings.Bounded;

   type Key_Type is
      record
         Row    : Rows.String_Type;
         Column : Columns.String_Type;
         Time   : Times.Number_Type;
      end record;

   type Context_Type is private;

   Is_Context_Free_Serialization : constant Boolean;

   function "<=" (Left, Right : Key_Type) return Boolean;
   function "="  (Left, Right : Key_Type) return Boolean;
   function Compare
     (Left, Right : Key_Type)
      return Utils.Comparison_Result_Type;

   function New_Context
      return Context_Type;

   function Size_Bound
     (Key : Key_Type)
      return IO.Blocks.Size_Type;

   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Key     : in     Key_Type);

   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Key     :    out Key_Type);

   procedure Skip
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type);

   function Short_Bound
     (Left  : Key_Type)
      return Key_Type;

   function Short_Delimiter
     (Left  : Key_Type;
      Right : Key_Type)
      return Key_Type;

private
   package Row_Serialization renames Rows.Uncompressed;
   package Column_Serialization renames Columns.Uncompressed;

   Is_Context_Free_Serialization : constant Boolean
      := Row_Serialization.Is_Context_Free_Serialization and
         Column_Serialization.Is_Context_Free_Serialization and
         Times.Is_Context_Free_Serialization;

   type Context_Type is
      record
         Row_Context    : Row_Serialization.Context_Type;
         Column_Context : Column_Serialization.Context_Type;
         Time_Context   : Times.Context_Type;
      end record;

   pragma Inline (New_Context);

end DB.Types.Keys;

