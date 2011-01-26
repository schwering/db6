-- Abstract:
--
-- Root package of the REST interface.
--
-- Copyright 2010--2011 Christoph Schwering

with DB.Types.Keys;

package REST is

   URL_Path_Error             : exception;
   Stream_Error               : exception;
   Malformed_Input_Data_Error : exception;
   Insertion_Error            : exception;

   Count_Param     : constant String := "n";
   Offset_Param    : constant String := "o";
   Reverse_Param   : constant String := "rev";
   From_Excl_Param : constant String := "excl1";
   To_Excl_Param   : constant String := "excl2";
   Yes_Value       : constant String := "y";
   Infinity_Row    : constant String := "_inf";
   Next_URL_Key    : constant String := "_next";

   function Make_Key
     (Key : String;
      Max : Boolean := False)
      return DB.Types.Keys.Key_Type;

end REST;

