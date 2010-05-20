-- Abstract:
--
-- Just provides a simple way to access the global pool object:
-- DB.Utils.Global_Pool.Global_Storage_Pool
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Storage_Pools;

package DB.Utils.Global_Pool is
   pragma Preelaborate;

   type Null_Record_Type is null record;
   type Some_Ref_Type is access Null_Record_Type;

   Global_Storage_Pool : System.Storage_Pools.Root_Storage_Pool'Class renames
      Some_Ref_Type'Storage_Pool;

end DB.Utils.Global_Pool;

