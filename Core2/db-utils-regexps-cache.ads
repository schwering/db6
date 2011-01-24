-- Abstract:
--
-- Adds a cache layer ontop of the Regexps package.
--
-- Copyright 2011 Christoph Schwering

package DB.Utils.Regexps.Cache is
   pragma Elaborate_Body;

   function Compile (Pattern : String; Glob : Boolean := False) return Regexp;

end DB.Utils.Regexps.Cache;

