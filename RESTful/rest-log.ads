-- Abstract:
--
-- The design of the logger is broken.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Exceptions;

package REST.Log is

   procedure Info (Msg : in String);

   procedure Error (Msg : in String);
   procedure Error (Exc : in Ada.Exceptions.Exception_Occurrence);

end REST.Log;

