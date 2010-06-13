-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Types.Keys is

   function "<" (Left, Right : Rows.String_Type) return Boolean
   renames Rows."<";
   function "=" (Left, Right : Rows.String_Type) return Boolean
   renames Rows."=";

   function "<=" (Left, Right : Key_Type) return Boolean is
   begin
      if Left.Row < Right.Row then
         return True;
      elsif Left.Row = Right.Row then
         --if Left.Column < Right.Column then
            --return True;
         --elsif Left.Column = Right.Column then
            return not (Left.Time < Right.Time);
         --end if;
      end if;
      return False;
   end "<=";


   function "=" (Left, Right : Key_Type) return Boolean
   is
      pragma Warnings (Off);
      use type Rows.String_Type;
      use type Columns.String_Type;
      use type Times.Number_Type;
      pragma Warnings (On);
   begin
      return Left.Row = Right.Row
         --and Left.Column = Right.Column
         and Left.Time = Right.Time;
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
      --C := Columns.Compare (Left.Column, Right.Column);
      --if C /= Utils.Equal then
         --return C;
      --end if;
      C := Times.Compare (Left.Time, Right.Time);
      case C is
         when Utils.Less    => return Utils.Greater;
         when Utils.Equal   => return Utils.Equal;
         when Utils.Greater => return Utils.Less;
      end case;
   end Compare;


   function Short_Bound
     (Left : Key_Type)
      return Key_Type is
   begin
      return Key_Type' (Row    => Rows.Short_Bound (Left.Row),
                        Column => Columns.Short_Bound (Left.Column),
                        Time   => Times.Short_Bound (Left.Time));
   end Short_Bound;


   function Short_Delimiter (Left, Right : Key_Type) return Key_Type
   is
      function "<" (L, R : Key_Type) return Boolean is
      begin
         return L <= R and L /= R;
      end "<";

      pragma Assert (Left <= Right);
      Key       : Key_Type;
      Row_Delim : constant Rows.String_Type :=
         Rows.Short_Delimiter (Left.Row, Right.Row);
   begin
      if Left.Row < Row_Delim then
         Key :=  Key_Type' (Row    => Row_Delim,
                            Column => Columns.Empty_String,
                            Time   => Times.Number_Type'Last);
      else
         declare
            Column_Delim : constant Columns.String_Type :=
               Columns.Short_Delimiter (Left.Column, Right.Column);
         begin
            --if Left.Column < Column_Delim then
            --   return Key_Type' (Row    => Row_Delim,
            --                     Column => Column_Delim,
            --                     Time   => Times.Number_Type'Last);
            --else
               Key :=  Key_Type' (Row    => Row_Delim,
                                  Column => Column_Delim,
                                  Time   => Times.Short_Delimiter (Left.Time,
                                                                   Right.Time));
            --end if;
         end;
      end if;
      pragma Assert (Left /= Right or else (Left = Key and Key = Right));
      pragma Assert (Left = Right or else (Left <= Key and Key < Right));
      return Key;
   end Short_Delimiter;


   function New_Read_Context return Read_Context_Type is
   begin
      return Read_Context_Type' (Row_Serialization.New_Read_Context,
                                 Column_Serialization.New_Read_Context,
                                 Times.New_Read_Context);
   end New_Read_Context;


   function New_Write_Context return Write_Context_Type is
   begin
      return Write_Context_Type' (Row_Serialization.New_Write_Context,
                                  Column_Serialization.New_Write_Context,
                                  Times.New_Write_Context);
   end New_Write_Context;


   function Size_Bound (Key : Key_Type) return Blocks.Size_Type
   is
      use type Blocks.Size_Type;
   begin
      return Row_Serialization.Size_Bound (Key.Row) +
           --Column_Serialization.Size_Bound (Key.Column) + 
             Times.Size_Bound (Key.Time);
   end Size_Bound;


   procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Key     : in     Key_Type) is
   begin
      Row_Serialization.Write (Context.Row_Context, Block, Cursor, Key.Row);
      --Column_Serialization.Write (Context.Column_Context, Block, Cursor,
                                  --Key.Column);
      Times.Write (Context.Time_Context, Block, Cursor, Key.Time);
   end Write;


   procedure Read
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Key     :    out Key_Type) is
   begin
      Row_Serialization.Read (Context.Row_Context, Block, Cursor, Key.Row);
      Key.Column := Columns.Empty_String;
      --Column_Serialization.Read (Context.Column_Context, Block, Cursor,
                                   --Key.Column);
      Times.Read (Context.Time_Context, Block, Cursor, Key.Time);
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
      ----Column_Serialization.Skip (Context.Column_Context, Block, Cursor);
      --Times.Skip (Context.Time_Context, Block, Cursor);
   end Skip;

end DB.Types.Keys;

