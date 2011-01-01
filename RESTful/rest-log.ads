-- Abstract:
--
-- The design of the logger is broken.
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Exceptions;

package REST.Log is

   generic
      Kind : in String;
   procedure Log_To_Stdout (Msg : in String);

   procedure Info (Msg : in String);
   procedure Error (Msg : in String);
   procedure Error (Exc : in Ada.Exceptions.Exception_Occurrence);

end REST.Log;

