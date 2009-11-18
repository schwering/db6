-- Abstract:
--
-- Volatile BTree.
--
-- Copyright 2008, 2009 Christoph Schwering

with System.Pool_Global;
with System.Storage_Pools;

use System.Pool_Global;
use System.Storage_Pools;

with DB.Gen_BTrees;
with DB.Types.Keys;
with DB.Types.Values;
with DB.IO.Blocks.Memory_IO;

package DB.Volatile_BTrees is new Gen_BTrees
  (Key_Type                      => Types.Keys.Key_Type,
   Key_Context_Type              => Types.Keys.Context_Type,
   Write_Key                     => Types.Keys.Write,
   Read_Key                      => Types.Keys.Read,
   Skip_Key                      => Types.Keys.Skip,
   "="                           => Types.Keys."=",
   "<="                          => Types.Keys."<=",
   Value_Type                    => Types.Values.Value_Type,
   Value_Context_Type            => Types.Values.Context_Type,
   Write_Value                   => Types.Values.Write,
   Read_Value                    => Types.Values.Read,
   Skip_Value                    => Types.Values.Skip,
   Is_Context_Free_Serialization => Types.Keys.Is_Context_Free_Serialization
                                 and Types.Values.Is_Context_Free_Serialization,
   Storage_Pool                  => Root_Storage_Pool'Class(Global_Pool_Object),
   Block_IO                      => IO.Blocks.Memory_IO.IO);
--pragma Preelaborate (DB.Volatile_BTrees);

