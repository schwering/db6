with DB.Types.Gen_Numbers;
with DB.IO.Blocks.Direct_IO;

package DB.Types.Values is new Gen_Numbers
  (IO.Blocks.Direct_IO.Valid_Address_Type);
pragma Preelaborate (DB.Types.Values);

