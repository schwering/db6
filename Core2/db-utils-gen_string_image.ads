-- Abstract:
--
-- Image (to-string) function any kinds of strings that does not type-checking.
--
-- Copyright 2008--2011 Christoph Schwering

generic
   type Object_Type is limited private;
function DB.Utils.Gen_String_Image (O : Object_Type) return String;
pragma Preelaborate (DB.Utils.Gen_String_Image);

