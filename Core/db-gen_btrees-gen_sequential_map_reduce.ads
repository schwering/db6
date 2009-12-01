-- Abstract:
--
-- An implementation of map / reduce that guarantees to handle the
-- elements in the right order.
--
-- References:
--
-- HCs paper, Google paper, Haskell?
--
-- Design Notes:
--
-- The elements are read in a single thread and put into a queue.
-- Multiple threads pop from that queue and consume the elements.
-- Each of these consumers performs three steps in each loop: (1) pop from
-- queue, (2) map and (3) reduce with global element.
-- Another queue is used to control that if consumer X pops before Y, then X
-- reduces before Y. Its implementation is not very straightforward (because
-- there is also a Global_Mutex for the mutual exclusion in the pop and the
-- reduce phases.
-- Note that to avoid deadlocks, the consumer queue (which controls which
-- consumer is allowed to enter the reduce phase) must have enough space to
-- hold all tasks (this guarantees that its put-operation is not blocking).
--
-- Copyright 2008, 2009 Christoph Schwering

generic
   type Element_Type is private;
   Neutral_Element : Element_Type;
   with function Map
          (Key   : Key_Type;
           Value : Value_Type)
           return Element_Type;
   with procedure Reduce
          (Left    : in out Element_Type;
           Right   : in     Element_Type);
procedure DB.Gen_BTrees.Gen_Sequential_Map_Reduce
  (Tree        : in out Tree_Type;
   Transaction : in out Transaction_Type'Class;
   Cursor      : in out Cursor_Type;
   Element     :    out Element_Type;
   State       :    out Result_Type);

