-- Abstract:
--
-- Time type.
--
-- Copyright 2008, 2009 Christoph Schwering

with DB.Types.Gen_Numbers;

package DB.Types.Times is new Gen_Numbers(Time_Type);
pragma Preelaborate (DB.Types.Times);

