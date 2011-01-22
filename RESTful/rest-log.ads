-- Abstract:
--
-- The design of the logger is broken.
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Exceptions;

with GNAT.Source_Info;

package REST.Log is

   generic
      Kind : in String;
   procedure Log_To_Stdout
     (Msg    : in String;
      Source : in String := GNAT.Source_Info.File;
      Line   : in Natural := GNAT.Source_Info.Line);

   procedure Info
     (Msg    : in String;
      Source : in String := GNAT.Source_Info.File;
      Line   : in Natural := GNAT.Source_Info.Line);


   procedure Error
     (Msg    : in String;
      Source : in String := GNAT.Source_Info.File;
      Line   : in Natural := GNAT.Source_Info.Line);


   procedure Error
     (Exc    : in Ada.Exceptions.Exception_Occurrence;
      Source : in String := GNAT.Source_Info.File;
      Line   : in Natural := GNAT.Source_Info.Line);

end REST.Log;

