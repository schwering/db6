-- Abstract:
--
-- Returns the object.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic
   type Item_Type (<>) is private;
function DB.Utils.Gen_Identity
  (Item : Item_Type)
   return Item_Type;
pragma Pure (DB.Utils.Gen_Identity);
pragma Inline (DB.Utils.Gen_Identity);

