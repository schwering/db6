package body Util.Gen_Events is

   procedure Wait (Event : in out Event_Type; Item : out Item_Type)
   is begin
      Event.Wait(Item);
   end Wait;

   procedure Signal (Event : in out Event_Type; Item : in Item_Type)
   is begin
      Event.Signal(Item);
   end Signal;

   protected body Event_Type is
      entry Wait (Item : out Item_Type) when Occured
      is begin
         Item := Event_Type.Item;
      end Wait;

      entry Signal (Item : in Item_Type) when True
      is begin
         if Occured = True then
            raise Event_Error;
         end if;
         Occured := True;
         Event_Type.Item := Item;
         --requeue Reset;
      end Signal;

      entry Reset when Event_Type.Wait'Count = 0
      is begin
         Occured := False;
      end Reset;
   end Event_Type;

end Util.Gen_Events;

