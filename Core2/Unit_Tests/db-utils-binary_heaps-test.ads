-- Abstract:
--
-- Binary heap test fixture.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils.Gen_Binary_Heaps.Gen_Test;
with DB.Utils.Binary_Heaps_Test_Utils;

package DB.Utils.Binary_Heaps.Test is new Binary_Heaps.Gen_Test
   (Initial_Item => Binary_Heaps_Test_Utils.Initial_Item,
    Succ         => Binary_Heaps_Test_Utils.Succ,
    Img          => Binary_Heaps_Test_Utils.Image);

