-- Abstract:
--
-- Binary heap test fixture.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.DSA.Utils.Gen_Binary_Heaps.Gen_Test;
with DB.DSA.Utils.Binary_Heaps_Test_Utils;

package DB.DSA.Utils.Binary_Heaps.Test is new Binary_Heaps.Gen_Test
   (Initial_Item => Binary_Heaps_Test_Utils.Initial_Item,
    Succ         => Binary_Heaps_Test_Utils.Succ,
    Img          => Binary_Heaps_Test_Utils.Image);

