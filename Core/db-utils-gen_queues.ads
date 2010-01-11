-- Abstract:
--
-- A generic thread-safe (protected) queue.
--
-- Design Notes:
--
-- (Content)
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic
   Queue_Size : in Positive;
   type Item_Type is private;
package DB.Utils.Gen_Queues is
   pragma Pure;

   type Queue_Type is limited private;

   procedure Enqueue (Q : in out Queue_Type; Item : in Item_Type);
   procedure Dequeue 
     (Q       : in out Queue_Type;
      Success :    out Boolean;
      Item    :    out Item_Type);
   procedure Mark_Final (Q : in out Queue_Type);
   function Is_Full (Q : Queue_Type) return Boolean;
   function Is_Empty (Q : Queue_Type) return Boolean;
   function Size (Q : Queue_Type) return Natural;

private
   subtype Index_Type is Natural range 0 .. Queue_Size;
   type Array_Type is array (Index_Type) of Item_Type;

   function Succ (I : Index_Type) return Index_Type;
   pragma Inline (Succ);

   protected type Queue_Type is 
      entry Enqueue (Item : in Item_Type);

      entry Dequeue (Success : in out Boolean; Item : out Item_Type);

      procedure Mark_Final;
      pragma Inline (Mark_Final);

      function Is_Full return Boolean;
      pragma Inline (Is_Full);

      function Is_Empty return Boolean;
      pragma Inline (Is_Empty);

      function Size return Natural;
      pragma Inline (Size);

   private
      Arr   : Array_Type;
      Head  : Index_Type := Index_Type'First;
      Tail  : Index_Type := Index_Type'First;
      Final : Boolean    := False;
   end Queue_Type;
end DB.Utils.Gen_Queues;

