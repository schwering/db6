with Ada.Exceptions;

package DB.Util.Traceback is
   --pragma Preelaborate;

   function Traceback_String return String;

   procedure Print_Traceback;

   procedure Print_Traceback (E : Ada.Exceptions.Exception_Occurrence);

end DB.Util.Traceback;

