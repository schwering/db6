-- Abstract:
--
-- The design of the logger is broken. Who is first, the error or the
-- registration of the handler? So currently, Push simply prints the message.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Exceptions;

package REST.Error_Log is

   procedure Push (Msg : in String);
   procedure Push (Exc : in Ada.Exceptions.Exception_Occurrence);
   function Has return Boolean;
   function Top return String;
   procedure Pop;

end REST.Error_Log;

