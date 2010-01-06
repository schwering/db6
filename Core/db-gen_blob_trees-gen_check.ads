-- Abstract:
--
-- Checks a map's BTree for consistency.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic
   with function Key_To_String (Key : Key_Type) return String;
   with function Value_To_String (Value : Value_Type) return String;
procedure DB.Gen_Blob_Trees.Gen_Check
  (Tree : in out Tree_Type);
--pragma Preelaborate (DB.Tables.Maps.Gen_Check);

