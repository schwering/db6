-- Abstract:
--
-- Checks a BTree for consistency.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic
   with function Key_To_String (Key : Keys.Key_Type) return String;
   pragma Unreferenced (Key_To_String);
   with function Value_To_String (Value : Values.Value_Type) return String;
   pragma Unreferenced (Value_To_String);
   with function Address_To_String (Address : Block_IO.Address_Type)
      return String;
procedure DB.Gen_BTrees.Gen_Check
  (Tree : in out Tree_Type);
pragma Preelaborate (DB.Gen_BTrees.Gen_Check);

