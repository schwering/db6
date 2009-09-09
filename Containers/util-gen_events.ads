generic
   type Item_Type is private;
package Util.Gen_Events is
   pragma Pure;

   type Event_Type is limited private;

   Event_Error : exception;

   procedure Wait (Event : in out Event_Type; Item : out Item_Type);
   procedure Signal (Event : in out Event_Type; Item : in Item_Type);

private
   protected type Event_Type is
      entry Wait (Item : out Item_Type);
      entry Signal (Item : in Item_Type);
   private
      entry Reset;
      Occured : Boolean := False;
      Item    : Item_Type;
   end Event_Type;

end Util.Gen_Events;

