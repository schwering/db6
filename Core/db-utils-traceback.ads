-- Abstract:
--
-- Traceback using GNAT libraries.
--
-- Copyright 2008, 2009 Christoph Schwering

with Ada.Exceptions;

package DB.Utils.Traceback is
   --pragma Preelaborate;

   function Traceback_String return String;

   procedure Print_Traceback;

   procedure Print_Traceback (E : Ada.Exceptions.Exception_Occurrence);

end DB.Utils.Traceback;

