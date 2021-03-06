-- Abstract:
--
-- Checks a BTree for consistency.
--
-- Copyright 2008--2011 Christoph Schwering

generic
   with function Key_To_String (Key : Keys.Key_Type) return String;
   pragma Unreferenced (Key_To_String);
   with function Value_To_String (Value : Values.Value_Type) return String;
   pragma Unreferenced (Value_To_String);
procedure DB.DSA.Gen_BTrees.Gen_Check
  (Tree : in out Tree_Type);
pragma Preelaborate (DB.DSA.Gen_BTrees.Gen_Check);

