with Ada.Unchecked_Conversion;

package body DB.Types.Values is

   function Size_Of
     (Value : Value_Type)
      return IO.Blocks.Size_Type
   is
      function Size_Of is new IO.Blocks.Size_Of(Value_Type);
   begin
      return Size_Of(Value);
   end Size_Of;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Value   : in     Value_Type)
   is
      pragma Unreferenced (Context);
      procedure Write is new IO.Blocks.Write(Value_Type);
   begin
      Write(Block, Cursor, Value);
   end Write;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Value   :    out Value_Type)
   is
      pragma Unreferenced (Context);
      procedure Read is new IO.Blocks.Read(Value_Type);
   begin
      Read(Block, Cursor, Value);
   end Read;


   procedure Skip
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type)
   is
      Value : Value_Type;
   begin
      Read(Context, Block, Cursor, Value);
   end Skip;


   function To_Storage_Array
     (Value : Value_Type)
      return System.Storage_Elements.Storage_Array
   is
      Len : constant System.Storage_Elements.Storage_Offset
          := IO.Blocks.Bits_To_Units(Value'Size);
      subtype Raw_Type is System.Storage_Elements.Storage_Array(1 .. Len);
      function Convert is new Ada.Unchecked_Conversion(Value_Type, Raw_Type);
   begin
      return Convert(Value);
   end To_Storage_Array;


   function From_Storage_Array
     (Arr : System.Storage_Elements.Storage_Array)
      return Value_Type
   is
      Value : Value_Type;
      Len : constant System.Storage_Elements.Storage_Offset
          := IO.Blocks.Bits_To_Units(Value'Size);
      subtype Raw_Type is System.Storage_Elements.Storage_Array(1 .. Len);
      function Convert is new Ada.Unchecked_Conversion(Raw_Type, Value_Type);
   begin
      Value := Convert(Arr);
      return Value;
   end From_Storage_Array;

end DB.Types.Values;

