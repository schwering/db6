-- Abstract:
--
-- Key type. A key consists of a row, a column and a timestamp.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Blocks;
with DB.Blocks.Gen_Keys_Signature;
with DB.Types.Strings.Bounded;
with DB.Types.Times;
with DB.Utils;

package DB.Types.Keys is
   pragma Elaborate_Body;

   package Rows                 renames Strings.Bounded;
   package Columns              renames Strings.Bounded;
   package Times                renames Types.Times;
   package Row_Serialization    renames Rows.Uncompressed;
   package Column_Serialization renames Columns.Uncompressed;
   package Time_Serialization   renames Types.Times.Serialization;


   type Key_Type is
      record
         Row    : Rows.String_Type;
         Column : Columns.String_Type;
         Time   : Times.Time_Type;
      end record;

   type Read_Context_Type is
      record
         Row_Context    : Row_Serialization.Read_Context_Type;
         Column_Context : Column_Serialization.Read_Context_Type;
         Time_Context   : Time_Serialization.Read_Context_Type;
      end record;

   type Write_Context_Type is
      record
         Row_Context    : Row_Serialization.Write_Context_Type;
         Column_Context : Column_Serialization.Write_Context_Type;
         Time_Context   : Time_Serialization.Write_Context_Type;
      end record;

   Is_Context_Free_Serialization : constant Boolean :=
      Row_Serialization.Is_Context_Free_Serialization and
      Column_Serialization.Is_Context_Free_Serialization and
      Time_Serialization.Is_Context_Free_Serialization;


   function Null_Key return Key_Type;

   function "<"  (Left, Right : Key_Type) return Boolean;
   function "<=" (Left, Right : Key_Type) return Boolean;
   function "="  (Left, Right : Key_Type) return Boolean;
   function Compare
     (Left, Right : Key_Type)
      return Utils.Comparison_Result_Type;

   function New_Read_Context return Read_Context_Type;

   function New_Write_Context return Write_Context_Type;

   function Size_Bound (Key : Key_Type) return Blocks.Size_Type;

   procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Key     : in     Key_Type);

   procedure Read
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Key     :    out Key_Type);

   procedure Skip
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type);

   function Hash_1 (K : Key_Type) return Utils.Hash_Type;
   function Hash_2 (K : Key_Type) return Utils.Hash_Type;
   function Hash_3 (K : Key_Type) return Utils.Hash_Type;
   function Hash_4 (K : Key_Type) return Utils.Hash_Type;

   function Image (Key : Key_Type) return String;

   package Keys_Signature is new Blocks.Gen_Keys_Signature
     (Key_Type           => Key_Type,
      Read_Context_Type  => Read_Context_Type,
      Write_Context_Type => Write_Context_Type);

private
   pragma Inline (Null_Key);
   pragma Inline (New_Read_Context);
   pragma Inline (New_Write_Context);

end DB.Types.Keys;

