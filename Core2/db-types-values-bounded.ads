-- Abstract:
--
-- Instance.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Types.Gen_Strings.Gen_Bounded;

package DB.Types.Values.Bounded is new Values.Gen_Bounded (Max_Length => 1024);
pragma Pure (DB.Types.Values.Bounded);

