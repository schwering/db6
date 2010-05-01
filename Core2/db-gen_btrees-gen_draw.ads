-- Abstract:
--
-- Draws a BTree for consistency.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic
   --with function Key_To_String (Key : Key_Type) return String;
   --with function Value_To_String (Value : Value_Type) return String;
   with function Address_To_String (Address : Block_IO.Address_Type)
      return String;
procedure DB.Gen_BTrees.Gen_Draw
  (Tree : in out Tree_Type);
--pragma Preelaborate (DB.Gen_BTrees.Gen_Draw);

