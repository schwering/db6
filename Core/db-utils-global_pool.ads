-- Abstract:
--
-- Just provides a simple way to access the global pool object:
-- DB.Utils.Global_Pool.Global'Storage_Pool
--
-- Copyright 2008, 2009 Christoph Schwering

package DB.Utils.Global_Pool is
   pragma Preelaborate;

   type Null_Record_Type is null record;
   type Global is access Null_Record_Type;

end DB.Utils.Global_Pool;

