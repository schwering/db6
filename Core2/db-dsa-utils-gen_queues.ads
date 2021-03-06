-- Abstract:
--
-- A generic thread-safe (protected) fixed-size queue.
--
-- Copyright 2008--2011 Christoph Schwering

generic
   Queue_Size : in Positive;
   type Item_Type is private;
package DB.DSA.Utils.Gen_Queues is
   pragma Pure;

   type Queue_Type is limited private;

   type Item_Array_Type is array (Positive range <>) of Item_Type;

   procedure Enqueue
     (Q    : in out Queue_Type;
      Item : in     Item_Type);
   -- Enqueues a new element or blocks until enough space is available.

   procedure Enqueue
     (Q     : in out Queue_Type;
      Items : in     Item_Array_Type);
   -- Enqueues all Items. This might involve multiple operations blocking that
   -- enqueue sub-arrays of Items.

   procedure Dequeue
     (Q       : in out Queue_Type;
      Item    :    out Item_Type;
      Success :    out Boolean);
   -- Dequeues an element. Blocks until a new element is available or the queue
   -- was marked as final. Success if true iff the queue is not final.

   procedure Dequeue
     (Q     : in out Queue_Type;
      Items :    out Item_Array_Type;
      Last  :    out Natural);
   -- Dequeues as many elements as possible from the queue into Items or blocks
   -- until either at least one element is available or the queue is marked as
   -- final. In the latter case, Last is set to Items'First - 1. However, it is
   -- not guaranteed that all dequeueable elements are dequeued.

   procedure Mark_Final (Q : in out Queue_Type);
   -- Marks the queue as final meaning that the producer won't enqueue anything
   -- anymore. The consequence is that active and subsequent Dequeue statements
   -- won't block anymore.

   function Is_Final (Q : Queue_Type) return Boolean;
   -- Indicates whether the queue is final and empty. Initially, this is false
   -- and gets true when someone first called Mark_Final and all items were
   -- dequeued (before or after Mark_Final).

   function Is_Full (Q : Queue_Type) return Boolean;
   function Is_Empty (Q : Queue_Type) return Boolean;
   function Size (Q : Queue_Type) return Natural;

private
   subtype Index_Type is Natural range 0 .. Queue_Size;

   function Succ (I : Index_Type) return Index_Type;
   pragma Inline (Succ);

   protected type Queue_Type is
      entry Enqueue (Item : in Item_Type);

      entry Enqueue (Items : in Item_Array_Type; Last : out Natural);

      entry Dequeue (Item : out Item_Type; Success : out Boolean);

      entry Dequeue (Items : out Item_Array_Type; Last : out Natural);

      procedure Mark_Final;
      pragma Inline (Mark_Final);

      function Is_Final return Boolean;
      pragma Inline (Is_Final);

      function Is_Full return Boolean;
      pragma Inline (Is_Full);

      function Is_Empty return Boolean;
      pragma Inline (Is_Empty);

      function Size return Natural;
      pragma Inline (Size);

   private
      Arr   : Item_Array_Type (Index_Type);
      Head  : Index_Type := Index_Type'First;
      Tail  : Index_Type := Index_Type'First;
      Final : Boolean    := False;
   end Queue_Type;
end DB.DSA.Utils.Gen_Queues;

