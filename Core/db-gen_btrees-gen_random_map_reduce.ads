-- Abstract:
--
-- An implementation of map / reduce that does not guarantee to handle the
-- elements in the right order.
--
-- References:
--
-- HCs paper, Google paper, Haskell?
--
-- Design Notes:
--
-- The elements are read, mapped and reduced by multiple threads in parallel.
-- They are 
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

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
   Default_Concurrency_Degree : in Positive := 10;
procedure DB.Gen_BTrees.Gen_Random_Map_Reduce
  (Tree               : in out Tree_Type;
   Transaction        : in out Transaction_Type'Class;
   Cursor             : in out Cursor_Type;
   Element            :    out Element_Type;
   State              :    out Result_Type;
   Concurrency_Degree : in     Positive := Default_Concurrency_Degree);

