-- Abstract:
--
-- Checks a map's BTree for consistency.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic
   with function Key_To_String (Key : Key_Type) return String;
   with function Value_To_String (Value : Types.Values.Bounded.String_Type)
      return String;
procedure DB.Tables.Maps.Gen_Check
  (Map : in out Map_Type);
--pragma Preelaborate (DB.Tables.Maps.Gen_Check);

