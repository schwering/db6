-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

with Ada.Unchecked_Conversion;

package body DB.Types.Gen_Numbers is

   function Size_Of
     (Number : Number_Type)
      return IO.Blocks.Size_Type
   is
      function Size_Of is new IO.Blocks.Size_Of(Number_Type);
   begin
      return Size_Of(Number);
   end Size_Of;


   procedure Write
     (Context : in out Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Number  : in     Number_Type)
   is
      pragma Unreferenced (Context);
      procedure Write is new IO.Blocks.Write(Number_Type);
   begin
      Write(Block, Cursor, Number);
   end Write;


   procedure Read
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Number  :    out Number_Type)
   is
      pragma Unreferenced (Context);
      procedure Read is new IO.Blocks.Read(Number_Type);
   begin
      Read(Block, Cursor, Number);
   end Read;


   procedure Skip
     (Context : in out Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type)
   is
      Number : Number_Type;
   begin
      Read(Context, Block, Cursor, Number);
   end Skip;


   function To_Storage_Array
     (Number : Number_Type)
      return System.Storage_Elements.Storage_Array
   is
      Len : constant System.Storage_Elements.Storage_Offset
          := IO.Blocks.Bits_To_Units(Number'Size);
      subtype Raw_Type is System.Storage_Elements.Storage_Array(1 .. Len);
      function Convert is new Ada.Unchecked_Conversion(Number_Type, Raw_Type);
   begin
      return Convert(Number);
   end To_Storage_Array;


   function From_Storage_Array
     (Arr : System.Storage_Elements.Storage_Array)
      return Number_Type
   is
      Number : Number_Type;
      Len : constant System.Storage_Elements.Storage_Offset
          := IO.Blocks.Bits_To_Units(Number'Size);
      subtype Raw_Type is System.Storage_Elements.Storage_Array(1 .. Len);
      function Convert is new Ada.Unchecked_Conversion(Raw_Type, Number_Type);
   begin
      Number := Convert(Arr);
      return Number;
   end From_Storage_Array;

end DB.Types.Gen_Numbers;

