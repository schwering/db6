-- Abstract:
--
-- Instance.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Types.Gen_Strings.Gen_Bounded;

package DB.Types.Strings.Bounded is new Strings.Gen_Bounded
  (Max_Length => 1024);
pragma Preelaborate (DB.Types.Strings.Bounded);

