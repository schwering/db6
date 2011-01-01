-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Maps.Tag_Map.Utils is

   procedure Store (State : out Tag_Map_State_Type) is
   begin
      State := (Sealed, Head, Tail, Map);
   end Store;


   procedure Restore (State : in Tag_Map_State_Type) is
   begin
      Clear;
      Sealed := State.Sealed;
      Head   := State.Head;
      Tail   := State.Tail;
      Map    := State.Map;
   end Restore;

end DB.Maps.Tag_Map.Utils;

