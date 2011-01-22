-- Abstract:
--
-- Instance.
-- See instance children Bounded and Unbounded.
--
-- Copyright 2008--2011 Christoph Schwering

--with System.Storage_Elements;

with DB.Types.Gen_Strings;

package DB.Types.Byte_Arrays is new Types.Gen_Strings
  (Item_Type => Character);
  --(Item_Type => System.Storage_Elements.Storage_Element);
pragma Pure (DB.Types.Byte_Arrays);

