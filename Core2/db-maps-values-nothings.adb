-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Maps.Values.Nothings is

   function New_Value
     (Params : not null access DB.Maps.Value_Parameters_Type)
      return Value_Type
   is
      pragma Unreferenced (Params);
   begin
      return (Maps.Value_Type with null record);
   end New_Value;


   function New_Value return Value_Type is
   begin
      return (Maps.Value_Type with null record);
   end New_Value;


   function Size_Bound
      (Value : Value_Type)
       return Ada.Streams.Stream_Element_Offset
   is
      pragma Unreferenced (Value);
   begin
      return 0;
   end Size_Bound;


   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : in              Value_Type) is
   begin
      null;
   end Write;


   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : out             Value_Type) is
   begin
      null;
   end Read;


   function Equals (A, B : Value_Type) return Boolean
   is
      pragma Unreferenced (A);
      pragma Unreferenced (B);
   begin
      return True;
   end Equals;


   function Image (Value : Value_Type) return String
   is
      pragma Unreferenced (Value);
   begin
      return "null";
   end Image;

end DB.Maps.Values.Nothings;

