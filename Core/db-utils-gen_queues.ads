-- Abstract:
--
-- A thread-safe, blocking (i.e. synchronized) fixed-size queue.
--
-- Design Notes:
--
-- Implemented as a protected type.
--
-- Copyright 2008, 2009 Christoph Schwering

generic
   type Item_Type is private;
   Size : in Positive;
package DB.Utils.Gen_Queues is
   pragma Pure;

   type Queue_Type is limited private;

   procedure Set_Final
     (Queue : in out Queue_Type);

   function Is_Final
     (Queue : Queue_Type)
      return Boolean;

   procedure Put
     (Queue : in out Queue_Type;
      Item  : in     Item_Type);

   procedure Pop
     (Queue : in out Queue_Type;
      Item  :    out Item_Type;
      Final :    out Boolean);

private
   type Index_Type is new Natural range 0 .. Size - 1;
   type Array_Type is array (Index_Type) of Item_Type;

   function "+" (L, R : Index_Type) return Index_Type;

   protected type Queue_Type is 
      procedure Set_Final;
      function Is_Final return Boolean;
      entry Put (Item : in Item_Type);
      entry Pop (Item : out Item_Type; Final : out Boolean);
   private
      Final : Boolean := False;
      Arr   : Array_Type;
      Head  : Index_Type := Index_Type'First;
      Tail  : Index_Type := Index_Type'First;
   end Queue_Type;
end DB.Utils.Gen_Queues;

