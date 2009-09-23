with DB.Gen_BTrees;
with DB.Types.Keys;
with DB.Types.Values;
with DB.IO.Blocks.CFS_IO;

package DB.CFS_BTrees is new Gen_BTrees
  (Key_Type                      => Types.Keys.Key_Type,
   Key_Context_Type              => Types.Keys.Context_Type,
   Write_Key                     => Types.Keys.Write,
   Read_Key                      => Types.Keys.Read,
   Skip_Key                      => Types.Keys.Skip,
   "="                           => Types.Keys."=",
   "<="                          => Types.Keys."<=",
   Value_Type                    => Types.Values.Number_Type,
   Value_Context_Type            => Types.Values.Context_Type,
   Write_Value                   => Types.Values.Write,
   Read_Value                    => Types.Values.Read,
   Skip_Value                    => Types.Values.Skip,
   Is_Context_Free_Serialization => Types.Keys.Is_Context_Free_Serialization
                                and Types.Values.Is_Context_Free_Serialization,
   Block_IO                      => IO.Blocks.CFS_IO.IO);
--pragma Preelaborate (DB.CFS_BTrees);

