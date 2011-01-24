-- Abstract:
--
-- Simple LRU cache test fixture.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.DSA.Utils.Gen_Small_LRU_Caches.Gen_Test;
with DB.DSA.Utils.Small_LRU_Caches_Test_Utils;

package DB.DSA.Utils.Small_LRU_Caches.Test is new Small_LRU_Caches.Gen_Test
  (Initial_Key   => Small_LRU_Caches_Test_Utils.Initial,
   Next_Key      => Small_LRU_Caches_Test_Utils.Succ,
   Key_Img       => Small_LRU_Caches_Test_Utils.Image,
   Initial_Value => Small_LRU_Caches_Test_Utils.Initial,
   Next_Value    => Small_LRU_Caches_Test_Utils.Succ,
   Value_Img     => Small_LRU_Caches_Test_Utils.Image);

