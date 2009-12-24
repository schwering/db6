-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

package body DB.Types.Keys is

   function "<" (Left, Right : Rows.String_Type) return Boolean
   renames Rows."<";
   function "=" (Left, Right : Rows.String_Type) return Boolean
   renames Rows."=";
   function "<" (Left, Right : Times.Number_Type) return Boolean
   renames Times."<";
   function "=" (Left, Right : Times.Number_Type) return Boolean
   renames Times."=";

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
      use type Rows.String_Type;
      use type Columns.String_Type;
      use type Times.Number_Type;
   begin
      return Left.Row = Right.Row
         --and Left.Column = Right.Column
         and Left.Time = Right.Time;
   end "=";


   function Size_Bound
     (Key : Key_Type)
      return IO.Blocks.Size_Type
   is
      use type IO.Blocks.Size_Type;
   begin
      return Row_Serialization.Size_Bound(Key.Row) +
           --Column_Serialization.Size_Bound(Key.Column) + 
             Times.Size_Bound(Key.Time);
   end Size_Bound;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Key     : in     Key_Type) is
   begin
      Row_Serialization.Write(Context.Row_Context, Block, Cursor, Key.Row);
      --Column_Serialization.Write(Context.Column_Context, Block, Cursor,
                                  --Key.Column);
      Times.Write(Context.Time_Context, Block, Cursor, Key.Time);
   end Write;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Key     :    out Key_Type) is
   begin
      Row_Serialization.Read(Context.Row_Context, Block, Cursor, Key.Row);
      Key.Column := Columns.Empty_String;
      --Column_Serialization.Read(Context.Column_Context, Block, Cursor,
                                   --Key.Column);
      Times.Read(Context.Time_Context, Block, Cursor, Key.Time);
   end Read;


   procedure Skip
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type)
   is
      Key : Key_Type;
   begin
      Read(Context, Block, Cursor, Key);
      --Row_Serialization.Skip(Context.Row_Context, Block, Cursor);
      ----Column_Serialization.Skip(Context.Column_Context, Block, Cursor);
      --Times.Skip(Context.Time_Context, Block, Cursor);
   end Skip;

end DB.Types.Keys;

