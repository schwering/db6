-- Abstract:
--
-- Instance.
-- See instance children Bounded and Unbounded.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Types.Gen_Strings;

package DB.Types.Strings is new Types.Gen_Strings (Item_Type => Character);
pragma Pure (DB.Types.Strings);

