-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

with Ada.Unchecked_Conversion;
with System;

package body DB.IO.Blocks is

   function To_Block
     (Block     : Long_Block_Type;
      Last_Used : Long_Position_Type)
      return Block_Type
   is
      B : Block_Type := Block(Index_Type);
   begin
      B(Last_Used + 1 .. B'Last) := (others => 0);
      return B;
   end To_Block;


   function To_Long_Block
     (Block : Block_Type)
      return Long_Block_Type
   is
      Long_Block : Long_Block_Type;
   begin
      Long_Block(Block'Range) := Block;
      return Long_Block;
   end To_Long_Block;


   function New_Cursor
     (Start : Base_Index_Type)
      return Cursor_Type is
   begin
      return Cursor_Type'(Pos => Start);
   end New_Cursor;


   function Is_Valid
     (Block  : Base_Block_Type;
      Cursor : Cursor_Type)
      return Boolean is
   begin
      return Cursor.Pos in Block'Range;
   end Is_Valid;


   function Position
     (Cursor : Cursor_Type)
      return Base_Position_Type is
   begin
      return Cursor.Pos;
   end Position;


   function Bits_To_Units
     (Bits : Size_Type)
      return Size_Type is
   begin
      return (Bits + System.Storage_Unit - 1) / System.Storage_Unit;
   end Bits_To_Units;


   procedure Reset
     (Block : in out Base_Block_Type) is
   begin
      for I in Block'Range loop
         Block(I) := Storage_Element_Type'First;
      end loop;
   end Reset;


   function Size_Of
     (Item  : Item_Type)
      return Size_Type is
   begin
      return Bits_To_Units(Item'Size);
   end Size_Of;


   procedure Write_At
     (Block      : in out Base_Block_Type;
      From_Index : in     Base_Index_Type;
      To_Index   : in     Base_Position_Type;
      Item       : in     Item_Type)
   is
      subtype Raw_Type is Base_Block_Type(From_Index .. To_Index);
      function Convert is new Ada.Unchecked_Conversion(Item_Type, Raw_Type);
   begin
      Block(From_Index .. To_Index) := Convert(Item);
   end Write_At;


   procedure Read_At
     (Block      : in  Base_Block_Type;
      From_Index : in  Base_Index_Type;
      To_Index   : in  Base_Position_Type;
      Item       : out Item_Type)
   is
      subtype Raw_Type is Base_Block_Type(From_Index .. To_Index);
      function Convert is new Ada.Unchecked_Conversion(Raw_Type, Item_Type);
   begin
      Item := Convert(Block(From_Index .. To_Index));
   end Read_At;


   procedure Write
     (Block  : in out Base_Block_Type;
      Cursor : in out Cursor_Type;
      Item   : in     Item_Type)
   is
      Len : constant Base_Position_Type
          := Base_Position_Type(Bits_To_Units(Item'Size));
      subtype Raw_Type is Base_Block_Type(1 .. Len);
      function Convert is new Ada.Unchecked_Conversion(Item_Type, Raw_Type);
   begin
      if Integer(Cursor.Pos) < Integer(Block'First) or
         Integer(Cursor.Pos) > Integer(Block'Last) or
         Integer(Cursor.Pos) + Integer(Len) - 1 < Integer(Block'First) or
         Integer(Cursor.Pos) + Integer(Len) - 1 > Integer(Block'Last) then
         Cursor.Pos := 0;
         pragma Assert (Cursor.Pos not in Block'Range);
         return;
      end if;
      Block(Cursor.Pos .. Cursor.Pos + Len - 1) := Convert(Item);
      if Integer(Cursor.Pos) + Integer(Len) <= Integer(Block'Last) then
         Cursor.Pos := Cursor.Pos + Len;
      else
         Cursor.Pos := Base_Position_Type'First;
      end if;
   end Write;


   procedure Read
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type;
      Item   :    out Item_Type)
   is
      Len : constant Base_Position_Type
          := Base_Position_Type(Bits_To_Units(Item'Size));
      subtype Raw_Type is Base_Block_Type(1 .. Len);
      function Convert is new Ada.Unchecked_Conversion(Raw_Type, Item_Type);
   begin
      if Integer(Cursor.Pos) < Integer(Block'First) or
         Integer(Cursor.Pos) > Integer(Block'Last) or
         Integer(Cursor.Pos) + Integer(Len) - 1 < Integer(Block'First) or
         Integer(Cursor.Pos) + Integer(Len) - 1 > Integer(Block'Last) then
         Cursor.Pos := 0;
         pragma Assert (Cursor.Pos not in Block'Range);
         return;
      end if;
      Item := Convert(Block(Cursor.Pos .. Cursor.Pos + Len - 1));
      if Integer(Cursor.Pos) + Integer(Len) <= Integer(Block'Last) then
         Cursor.Pos := Cursor.Pos + Len;
      else
         Cursor.Pos := Base_Position_Type'First;
      end if;
   end Read;


   function Size_Of_Array
     (Arr : Array_Type;
      To  : Index_Type'Base := Array_Type'Last)
      return Size_Type
   is
      function Size_Of_Index is new Size_Of(Index_Type'Base);
      type Array_Sub_Type is array (Arr'First .. To) of Item_Type;
      function Size_Of_Data is new Size_Of(Array_Sub_Type);
   begin
      return Size_Of_Index(To)
           + Size_Of_Data(Array_Sub_Type(Arr(Arr'First .. To)));
   end Size_Of_Array;


   procedure Write_Array
     (Block  : in out Base_Block_Type;
      Cursor : in out Cursor_Type;
      Arr    : in     Array_Type;
      To     : in     Index_Type'Base := Array_Type'Last)
   is
      procedure Write_Index is new Write(Index_Type'Base);
      type Array_Sub_Type is array (Arr'First .. To) of Item_Type;
      procedure Write_Data is new Write(Array_Sub_Type);
   begin
      Write_Index(Block, Cursor, To);
      if Is_Valid(Block, Cursor) then
         Write_Data(Block, Cursor, Array_Sub_Type(Arr(Arr'First .. To)));
      end if;
   end Write_Array;


   procedure Read_Array
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type;
      Arr    :    out Array_Type;
      To     :    out Index_Type'Base)
   is
      procedure Read_Index is new Read(Index_Type'Base);
   begin
      Read_Index(Block, Cursor, To);
      if Is_Valid(Block, Cursor) then
         declare
            type Array_Sub_Type is array (Arr'First .. To) of Item_Type;
            procedure Read_Data is new Read(Array_Sub_Type);
         begin
            Read_Data(Block, Cursor, Array_Sub_Type(Arr(Arr'First .. To)));
         end;
      end if;
   end Read_Array;

end DB.IO.Blocks;

