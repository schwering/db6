-- Abstract:
--
-- Utilities to store and restore a state of the tag map.
-- When a state is restored, the current state is freed. This means that the
-- following doesn't work:
--  * S1 <- store
--    ...
--  * S2 <- store
--  * restore(S1)
--    ...
--  * restore(S2)    <----   S2's memory has been freed
--
-- Copyright 2008--2011 Christoph Schwering

package DB.Maps.Tag_Map.Utils is

   type Tag_Map_State_Type is private;

   procedure Store (State : out Tag_Map_State_Type);
   procedure Restore (State : in Tag_Map_State_Type);

private
   type Tag_Map_State_Type is
      record
         Sealed : Boolean;
         Head   : Node_Ref_Type;
         Tail   : Node_Ref_Type;
         Map    : Map_Type;
      end record;

end DB.Maps.Tag_Map.Utils;

