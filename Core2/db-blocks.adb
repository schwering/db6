-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Conversion;
with System;

package body DB.Blocks is

   function To_Block
     (Block     : Base_Block_Type;
      Last_Used : Base_Position_Type)
      return Block_Type
   is
      B : Block_Type := Block (Index_Type);
   begin
      B (Last_Used + 1 .. B'Last) := (others => 0);
      return B;
   end To_Block;


   function To_Block
     (Block  : Base_Block_Type;
      Cursor : Cursor_Type)
      return Block_Type
   is
      B : Block_Type := Block (Index_Type);
   begin
      if Is_Valid (Cursor) then
         B (Position (Cursor) .. B'Last) := (others => 0);
      end if;
      return B;
   end To_Block;


   procedure Reset_Free_Space_Of_Block
     (Block     : in out Base_Block_Type;
      Last_Used : in     Base_Position_Type) is
   begin
      Block (Last_Used + 1 .. Block_Type'Last) := (others => 0);
   end Reset_Free_Space_Of_Block;


   function New_Cursor (Start : Base_Position_Type) return Cursor_Type
   is
      pragma Precondition (Start /= Invalid_Position);
   begin
      return Cursor_Type' (Pos => Start);
   end New_Cursor;


   function Is_Valid (Cursor : Cursor_Type) return Boolean is
   begin
      return Is_Valid (Cursor.Pos);
   end Is_Valid;


   function Is_Valid (Position : Base_Position_Type) return Boolean is
   begin
      return Position /= Invalid_Position;
   end Is_Valid;


   function Position (Cursor : Cursor_Type) return Base_Position_Type is
   begin
      return Cursor.Pos;
   end Position;


   function Remaining_Space
     (Block  : Base_Block_Type;
      Cursor : Cursor_Type)
      return Size_Type
   is
      pragma Precondition (Is_Valid (Cursor));
   begin
      return Size_Type (Block'Last - Cursor.Pos + 1);
   end Remaining_Space;


   function Moved_Since
     (Cursor : Cursor_Type;
      Since  : Base_Position_Type)
      return Size_Type
   is
      pragma Precondition (Is_Valid (Cursor));
   begin
      return Size_Type (Cursor.Pos) - Size_Type (Since);
   end Moved_Since;


   function Bits_To_Units (Bits : Size_Type) return Size_Type is
   begin
      return (Bits + System.Storage_Unit - 1) / System.Storage_Unit;
   end Bits_To_Units;


   procedure Reset (Block : in out Base_Block_Type) is
   begin
      Block (Block'Range) := (others => Storage_Element_Type'First);
   end Reset;


   function Size_Of (Item : Item_Type) return Size_Type is
   begin
      return Bits_To_Units (Item'Size);
   end Size_Of;


   procedure Write_At
     (Block      : in out Base_Block_Type;
      From_Index : in     Base_Index_Type;
      To_Index   : in     Base_Position_Type;
      Item       : in     Item_Type)
   is
      subtype Raw_Type is Base_Block_Type (From_Index .. To_Index);
      function Convert is new Ada.Unchecked_Conversion (Item_Type, Raw_Type);
   begin
      Block (From_Index .. To_Index) := Convert (Item);
   end Write_At;


   procedure Read_At
     (Block      : in  Base_Block_Type;
      From_Index : in  Base_Index_Type;
      To_Index   : in  Base_Position_Type;
      Item       : out Item_Type)
   is
      subtype Raw_Type is Base_Block_Type (From_Index .. To_Index);
      function Convert is new Ada.Unchecked_Conversion (Raw_Type, Item_Type);
   begin
      Item := Convert (Block (From_Index .. To_Index));
   end Read_At;


   procedure Write
     (Block  : in out Base_Block_Type;
      Cursor : in out Cursor_Type;
      Item   : in     Item_Type)
   is
      Len : constant Base_Position_Type :=
         Base_Position_Type (Bits_To_Units (Item'Size));
      subtype Raw_Type is Base_Block_Type (1 .. Len);
      function Convert is new Ada.Unchecked_Conversion (Item_Type, Raw_Type);
   begin
      declare
         subtype Block_Range is
            Integer range Integer (Block'First) ..  Integer (Block'Last);
      begin
         if Integer (Cursor.Pos) not in Block_Range or
            Integer (Cursor.Pos) + Integer (Len) - 1 not in Block_Range then
            Cursor.Pos := Invalid_Position;
            pragma Assert (Cursor.Pos not in Block'Range);
            return;
         end if;
      end;
      Block (Cursor.Pos .. Cursor.Pos + Len - 1) := Convert (Item);
      Cursor.Pos := Cursor.Pos + Len;
   end Write;


   procedure Read
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type;
      Item   :    out Item_Type)
   is
      Len : constant Base_Position_Type :=
         Base_Position_Type (Bits_To_Units (Item'Size));
      subtype Raw_Type is Base_Block_Type (1 .. Len);
      function Convert is new Ada.Unchecked_Conversion (Raw_Type, Item_Type);
   begin
      declare
         subtype Block_Range is
            Integer range Integer (Block'First) .. Integer (Block'Last);
      begin
         if Integer (Cursor.Pos) not in Block_Range or
            Integer (Cursor.Pos) + Integer (Len) - 1 not in Block_Range then
            Cursor.Pos := Invalid_Position;
            pragma Assert (Cursor.Pos not in Block'Range);
            return;
         end if;
      end;
      Item := Convert (Block (Cursor.Pos .. Cursor.Pos + Len - 1));
      Cursor.Pos := Cursor.Pos + Len;
   end Read;


   procedure Skip
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type)
   is
      Len : constant Base_Position_Type :=
         Base_Position_Type (Bits_To_Units (Item_Type'Size));
   begin
      declare
         subtype Block_Range is
            Integer range Integer (Block'First) ..  Integer (Block'Last);
      begin
         if Integer (Cursor.Pos) not in Block_Range or
            Integer (Cursor.Pos) + Integer (Len) - 1 not in Block_Range then
            Cursor.Pos := Invalid_Position;
            pragma Assert (Cursor.Pos not in Block'Range);
            return;
         end if;
      end;
      Cursor.Pos := Cursor.Pos + Len;
   end Skip;


   function Size_Of_Array
     (Arr  : Array_Type;
      From : Index_Type;
      To   : Index_Type'Base)
      return Size_Type
   is
      pragma Precondition (From <= Index_Type'Base'Succ (To));
      pragma Precondition (From in Arr'Range);
      function Size_Of_Index is new Size_Of (Index_Type'Base);
      type Array_Sub_Type is array (From .. To) of Item_Type;
      function Size_Of_Data is new Size_Of (Array_Sub_Type);
   begin
      if From > To then
         return Size_Of_Index (To);
      end if;
      pragma Assert (To in Arr'Range);
      return Size_Of_Index (To) +
             Size_Of_Data (Array_Sub_Type (Arr (From .. To)));
   end Size_Of_Array;


   procedure Write_Array
     (Block  : in out Base_Block_Type;
      Cursor : in out Cursor_Type;
      Arr    : in     Array_Type;
      From   : in     Index_Type;
      To     : in     Index_Type'Base)
   is
      pragma Precondition (From <= Index_Type'Base'Succ (To));
      pragma Precondition (From in Arr'Range);
      procedure Write_Index is new Write (Index_Type'Base);
      type Array_Sub_Type is array (From .. To) of Item_Type;
      procedure Write_Data is new Write (Array_Sub_Type);
   begin
      Write_Index (Block, Cursor, To);
      if From > To then
         return;
      end if;
      pragma Assert (To in Arr'Range);
      if Is_Valid (Cursor) then
         Write_Data (Block, Cursor, Array_Sub_Type (Arr (From .. To)));
      end if;
   end Write_Array;


   procedure Read_Array
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type;
      Arr    : in out Array_Type;
      From   : in     Index_Type;
      To     :    out Index_Type'Base)
   is
      procedure Read_Index is new Read (Index_Type'Base);
   begin
      Read_Index (Block, Cursor, To);
      pragma Assert (From <= Index_Type'Base'Succ (To));
      pragma Assert (From in Arr'Range);
      if From > To then
         return;
      end if;
      pragma Assert (To in Arr'Range);
      if Is_Valid (Cursor) then
         declare
            type Array_Sub_Type is array (From .. To) of Item_Type;
            procedure Read_Data is new Read (Array_Sub_Type);
         begin
            Read_Data (Block, Cursor, Array_Sub_Type (Arr (From .. To)));
         end;
      end if;
   end Read_Array;


   procedure Skip_Array
     (Block  : in     Base_Block_Type;
      Cursor : in out Cursor_Type;
      From   : in     Index_Type)
   is
      procedure Read_Index is new Read (Index_Type'Base);
      To : Index_Type'Base;
   begin
      Read_Index (Block, Cursor, To);
      if Is_Valid (Cursor) then
         declare
            type Array_Sub_Type is array (From .. To) of Item_Type;
            procedure Skip_Data is new Skip (Array_Sub_Type);
         begin
            Skip_Data (Block, Cursor);
         end;
      end if;
   end Skip_Array;

end DB.Blocks;

