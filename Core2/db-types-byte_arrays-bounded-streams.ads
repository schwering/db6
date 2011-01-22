-- Abstract:
--
-- Instance.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Types.Gen_Strings.Gen_Bounded.Gen_Streams;

package DB.Types.Byte_Arrays.Bounded.Streams is
   new Byte_Arrays.Bounded.Gen_Streams;
pragma Preelaborate (DB.Types.Byte_Arrays.Bounded.Streams);

