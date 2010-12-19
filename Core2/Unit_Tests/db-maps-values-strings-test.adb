-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Streams; use Ada.Streams;

with DB.Blocks; use DB.Blocks;
with DB.Blocks.Streams; use DB.Blocks.Streams;

with AUnit.Assertions; use AUnit.Assertions;

package body DB.Maps.Values.Strings.Test is

   procedure Test_Write_Read (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      Value  : constant Value_Type := New_Value ("huhu du kleiner Piepel");
      Value2 : Value_Type;
      Block  : aliased Base_Block_Type := Block_Type'(others => 0);
      Stream : aliased Stream_Type := New_Stream (Block'Unrestricted_Access);
   begin
      Stream.Seek (New_Cursor (Block'First));
      Write (Stream'Access, Value);
      Stream.Seek (New_Cursor (Block'First));
      Read (Stream'Access, Value2);
      Assert (Value.Equals (Value2), "Different values: "&
              "'"& Value.Image &"' /= "& "'"& Value2.Image &"'");
   end;


   package Size_Streams is
      type Stream_Type is new Root_Stream_Type with
         record
            Size : Ada.Streams.Stream_Element_Offset := 0;
         end record;

      overriding
      procedure Write
        (Stream : in out Stream_Type;
         Item   : in     Stream_Element_Array);

      overriding
      procedure Read
        (Stream : in out Stream_Type;
         Item   :    out Stream_Element_Array;
         Last   :    out Stream_Element_Offset)
      is null;
   end Size_Streams;


   package body Size_Streams is
      procedure Write
        (Stream : in out Stream_Type;
         Item   : in     Stream_Element_Array)
      is
         use type Size_Type;
      begin
         Stream.Size := Stream.Size + Item'Length;
      end Write;
   end Size_Streams;


   procedure Test_Size_Bound (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      Value       : constant Value_Type := New_Value ("huhu du kleiner Piepel");
      Size_Stream : aliased Size_Streams.Stream_Type;
      Size_Bound  : constant Ada.Streams.Stream_Element_Offset :=
         Value.Size_Bound;
   begin
      Write (Size_Stream'Access, Value);
      Assert (Size_Bound = Size_Stream.Size, "Size bound ="& Size_Bound'Img &
              " and written size "& Size_Stream.Size'Img &" differ");
   end;

end DB.Maps.Values.Strings.Test;

