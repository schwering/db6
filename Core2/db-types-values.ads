-- Abstract:
--
-- Instance.
-- See instance children Bounded and Unbounded.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Storage_Elements;

with DB.Types.Gen_Strings;

package DB.Types.Values is new Types.Gen_Strings
  (System.Storage_Elements.Storage_Element);
pragma Pure (DB.Types.Values);

