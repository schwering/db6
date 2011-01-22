-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Types.Nothings is

   function Compare
     (Left, Right : Nothing_Type)
      return Utils.Comparison_Result_Type is
   begin
      return Utils.Equal;
   end Compare;


   function New_Read_Context return Read_Context_Type is
   begin
      return (null record);
   end New_Read_Context;


   function New_Write_Context return Write_Context_Type is
   begin
      return (null record);
   end New_Write_Context;


   function Size_Bound (Nothing : Nothing_Type) return Blocks.Size_Type is
   begin
      return 0;
   end Size_Bound;


   function Image (Nothing : Nothing_Type) return String
   is
      pragma Unreferenced (Nothing);
   begin
      return "(null)";
   end Image;

end DB.Types.Nothings;

