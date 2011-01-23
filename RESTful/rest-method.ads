-- Abstract:
--
-- Root package of the handlers for the different HTTP methods.
--
-- Copyright 2010--2011 Christoph Schwering

with DB.Types.Keys;

package REST.Method is

   From_Param      : constant String := "q";
   Count_Param     : constant String := "n";
   Reverse_Param   : constant String := "rev";
   From_Excl_Param : constant String := "excl1";
   To_Excl_Param   : constant String := "excl2";
   Yes_Value       : constant String := "y";
   Infinity_Row    : constant String := "_inf";

   function Make_Key
     (Key : String;
      Max : Boolean := False)
      return DB.Types.Keys.Key_Type;

end REST.Method;

