-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks;

package body DB.Maps.Values.Gen_Floats is

   function New_Value
     (Params : not null access DB.Maps.Value_Parameters_Type)
      return Value_Type
   is
      pragma Unreferenced (Params);
   begin
      return (Maps.Value_Type with
              Real => 0.0);
   end New_Value;


   function New_Value (F : Float_Type) return Value_Type is
   begin
      return (Maps.Value_Type with
              Real => F);
   end New_Value;


   function Size_Bound
      (Value : Value_Type)
       return Ada.Streams.Stream_Element_Offset
   is
      pragma Unreferenced (Value);
   begin
      return Ada.Streams.Stream_Element_Offset
         (Blocks.Bits_To_Units (Float_Type'Size));
   end Size_Bound;


   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : in              Value_Type) is
   begin
      Float_Type'Write (Stream, Value.Real);
   end Write;


   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : out             Value_Type) is
   begin
      Float_Type'Read (Stream, Value.Real);
   end Read;


   function Equals (A, B : Value_Type) return Boolean is
   begin
      return A.Real = B.Real;
   end Equals;


   function Image (Value : Value_Type) return String
   is
      S : constant String := Float_Type'Image (Value.Real);
      F : Positive := S'First;
   begin
      while S (F) = ' ' loop
         F := F + 1;
      end loop;
      return S (F .. S'Last);
   end Image;

end DB.Maps.Values.Gen_Floats;

