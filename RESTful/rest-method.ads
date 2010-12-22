-- Abstract:
--
-- Root package of the handlers for the different HTTP methods.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Maps;

package REST.Method is

   From_Param    : constant String := "q";
   Count_Param   : constant String := "n";
   Reverse_Param : constant String := "rev";
   Incl_Param    : constant String := "incl";

   function Make_Key
     (Key : String;
      Max : Boolean := False)
      return DB.Maps.Key_Type;

end REST.Method;

