-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Types.Keys is

   function Null_Key return Key_Type is
   begin
      return (Rows.Empty_String, Columns.Empty_String, Times.Time_Type'Last);
   end Null_Key;


   function "<" (Left, Right : Rows.String_Type) return Boolean
   renames Rows."<";
   function "=" (Left, Right : Rows.String_Type) return Boolean
   renames Rows."=";


   function "<" (Left, Right : Key_Type) return Boolean
   is
      use type Times.Time_Type;
   begin
      if Left.Row < Right.Row then
         return True;
      elsif Left.Row = Right.Row then
         if Left.Column < Right.Column then
            return True;
         elsif Left.Column = Right.Column then
            return Right.Time < Left.Time;
            -- intentionally L > R (most recent time is smallest)
         end if;
      end if;
      return False;
   end "<";


   function "<=" (Left, Right : Key_Type) return Boolean
   is
      use type Times.Time_Type;
   begin
      if Left.Row < Right.Row then
         return True;
      elsif Left.Row = Right.Row then
         if Left.Column < Right.Column then
            return True;
         elsif Left.Column = Right.Column then
            return not (Left.Time < Right.Time);
            -- intentionally !(L < R) <=> L >= R (most recent time is smallest)
         end if;
      end if;
      return False;
   end "<=";


   function "=" (Left, Right : Key_Type) return Boolean
   is
      pragma Warnings (Off);
      use type Rows.String_Type;
      use type Columns.String_Type;
      use type Times.Time_Type;
      pragma Warnings (On);
   begin
      return Left.Row = Right.Row and then
             Left.Column = Right.Column and then
             Left.Time = Right.Time;
   end "=";


   function Compare (Left, Right : Key_Type) return Utils.Comparison_Result_Type
   is
      use type Utils.Comparison_Result_Type;
      C : Utils.Comparison_Result_Type;
   begin
      C := Rows.Compare (Left.Row, Right.Row);
      if C /= Utils.Equal then
         return C;
      end if;
      C := Columns.Compare (Left.Column, Right.Column);
      if C /= Utils.Equal then
         return C;
      end if;
      C := Time_Serialization.Compare (Left.Time, Right.Time);
      case C is
         when Utils.Less    => return Utils.Greater;
         when Utils.Equal   => return Utils.Equal;
         when Utils.Greater => return Utils.Less;
      end case;
   end Compare;


   function New_Read_Context return Read_Context_Type is
   begin
      return Read_Context_Type'(Row_Serialization.New_Read_Context,
                                Column_Serialization.New_Read_Context,
                                Time_Serialization.New_Read_Context);
   end New_Read_Context;


   function New_Write_Context return Write_Context_Type is
   begin
      return Write_Context_Type'(Row_Serialization.New_Write_Context,
                                 Column_Serialization.New_Write_Context,
                                 Time_Serialization.New_Write_Context);
   end New_Write_Context;


   function Size_Bound (Key : Key_Type) return Blocks.Size_Type
   is
      use type Blocks.Size_Type;
   begin
      return Row_Serialization.Size_Bound (Key.Row) +
             Column_Serialization.Size_Bound (Key.Column) +
             Time_Serialization.Size_Bound (Key.Time);
   end Size_Bound;


   procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Key     : in     Key_Type) is
   begin
      Row_Serialization.Write (Context.Row_Context, Block, Cursor, Key.Row);
      Column_Serialization.Write (Context.Column_Context, Block, Cursor,
                                  Key.Column);
      Time_Serialization.Write (Context.Time_Context, Block, Cursor, Key.Time);
   end Write;


   procedure Read
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Key     :    out Key_Type) is
   begin
      Row_Serialization.Read (Context.Row_Context, Block, Cursor, Key.Row);
      Column_Serialization.Read (Context.Column_Context, Block, Cursor,
                                 Key.Column);
      Time_Serialization.Read (Context.Time_Context, Block, Cursor, Key.Time);
   end Read;


   procedure Skip
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type)
   is
      Key : Key_Type;
   begin
      Read (Context, Block, Cursor, Key);
      --Row_Serialization.Skip (Context.Row_Context, Block, Cursor);
      --Column_Serialization.Skip (Context.Column_Context, Block, Cursor);
      --Time_Serialization.Skip (Context.Time_Context, Block, Cursor);
   end Skip;


   function Hash_1 (K : Key_Type) return Utils.Hash_Type
   is
      use type Utils.Hash_Type;
   begin
      return Rows.Hash_1 (K.Row) xor Columns.Hash_1 (K.Column);
   end Hash_1;


   function Hash_2 (K : Key_Type) return Utils.Hash_Type
   is
      use type Utils.Hash_Type;
   begin
      return Rows.Hash_2 (K.Row) xor Columns.Hash_2 (K.Column);
   end Hash_2;


   function Hash_3 (K : Key_Type) return Utils.Hash_Type
   is
      use type Utils.Hash_Type;
   begin
      return Rows.Hash_3 (K.Row) xor Columns.Hash_3 (K.Column);
   end Hash_3;


   function Hash_4 (K : Key_Type) return Utils.Hash_Type
   is
      use type Utils.Hash_Type;
   begin
      return Rows.Hash_4 (K.Row) xor Columns.Hash_4 (K.Column);
   end Hash_4;


   function Image (Key : Key_Type) return String is
   begin
      return "('"& String (Rows.To_Buffer (Key.Row)) &"', "&
              "'"& String (Columns.To_Buffer (Key.Column)) &"', "&
                   Times.Time_Type'Image (Key.Time) &")";
   end Image;

end DB.Types.Keys;

