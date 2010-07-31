-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks;

package body DB.Maps.Value_Utils.Integer_Values is

   function New_Value
     (Params : not null access DB.Maps.Value_Parameters_Type)
      return Value_Type
   is
      pragma Unreferenced (Params);
   begin
      return (Maps.Value_Type with
              Int => 0);
   end New_Value;


   function New_Value (I : Integer) return Value_Type is
   begin
      return (Maps.Value_Type with
              Int => I);
   end New_Value;


   function Size_Bound
      (Value : Value_Type)
       return Ada.Streams.Stream_Element_Offset is
   begin
      return Ada.Streams.Stream_Element_Offset
         (Blocks.Bits_To_Units (Integer'Size));
   end Size_Bound;


   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : in              Value_Type) is
   begin
      Integer'Write (Stream, Value.Int);
   end Write;


   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : out             Value_Type) is
   begin
      Integer'Read (Stream, Value.Int);
   end Read;


   function Equals (A, B : Value_Type) return Boolean is
   begin
      return A.Int = B.Int;
   end Equals;


   function Image (Value : Value_Type) return String
   is
      S : constant String := Integer'Image (Value.Int);
      F : Positive := S'First;
   begin
      while S (F) = ' ' loop
         F := F + 1;
      end loop;
      return S (F .. S'Last);
   end Image;

end DB.Maps.Value_Utils.Integer_Values;

