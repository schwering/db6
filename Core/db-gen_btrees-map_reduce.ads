-- Abstract:
--
-- Very simple map reduce implementation.
--
-- References:
--
-- HCs paper, Google paper, Haskell?
--
-- Design Notes:
--
-- blabla
--
-- Copyright 2008, 2009 Christoph Schwering

generic
package DB.Gen_BTrees.Map_Reduce is

   Global_Default_Concurrency_Degree : constant Positive := 10;

   ----------
   -- Random map reduce.

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
      Default_Concurrency_Degree : in Positive
         := Global_Default_Concurrency_Degree;
   procedure Gen_Random_Map_Reduce
     (Tree               : in out Tree_Type;
      Transaction        : in out Transaction_Type'Class;
      Cursor             : in out Cursor_Type;
      Element            :    out Element_Type;
      State              :    out Result_Type;
      Concurrency_Degree : in     Positive := Default_Concurrency_Degree);
   -- Maps the items from the cursor and reduces them in (more or less) random
   -- order (due to multithreading).

   generic
      type Element_Type (<>) is private;
      Neutral_Element : Element_Type;
      with function Map
             (Key   : Key_Type;
              Value : Value_Type)
              return Element_Type;
      with procedure Reduce
             (Left    : in out Element_Type;
              Right   : in     Element_Type);
      Default_Concurrency_Degree : in Positive
         := Global_Default_Concurrency_Degree;
   procedure Gen_Constrained_Random_Map_Reduce
     (Tree               : in out Tree_Type;
      Transaction        : in out Transaction_Type'Class;
      Cursor             : in out Cursor_Type;
      Element            :    out Element_Type;
      State              :    out Result_Type;
      Concurrency_Degree : in     Positive := Default_Concurrency_Degree);
   -- Same as Gen_Random_Map_Reduce, but Element_Type can be constrained.
   -- This requires a little bit heap usage.

   ----------
   -- Sequential map reduce.

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
      Default_Concurrency_Degree : in Positive
         := Global_Default_Concurrency_Degree;
   procedure Gen_Sequential_Map_Reduce
     (Tree               : in out Tree_Type;
      Transaction        : in out Transaction_Type'Class;
      Cursor             : in out Cursor_Type;
      Element            :    out Element_Type;
      State              :    out Result_Type;
      Concurrency_Degree : in     Positive := Default_Concurrency_Degree);
   -- Maps the items from the cursor and reduces them in strictly sequential
   -- order.

end DB.Gen_BTrees.Map_Reduce;

