-- Abstract:
--
-- Parser for URL paths. These paths have the form /foo/bar/bla.
-- Iterating is done as follows:
--
-- declare
--    P : constant String := AWS.URL.Pathname (URL);
--    I : Path_Parsers.Iterator_Type;
-- begin
--    loop
--       Path_Parsers.Next (P, I);
--       exit when Path_Parsers.Is_Final (I);
--       Do_Something_With (Path_Parsers.Value (P, I));
--    end loop;
-- end;
--
-- The visited elements for /foo/bar/bla are foo, bar and then bla.
--
-- Copyright 2008--2011 Christoph Schwering

with AWS.URL;

package REST.Path_Parsers is

   type Iterator_Type is private;

   procedure Next (Path : in String; Iterator : in out Iterator_Type);
   -- Moves the iterator to the next element. Even before the first time Value
   -- is called, Next must be called to calibrate the iterator.

   function Value (Path : String; Iterator : Iterator_Type) return String;
   -- Returns the value at the current position of the iterator. Before the
   -- first call of Value, Next must be called.

   function Is_Final (Iterator : Iterator_Type) return Boolean;
   -- Indicates that the iterator is final and no more Next or Value calls may
   -- be done.

   function Element
     (URL     : AWS.URL.Object;
      N       : Positive;
      Default : String := "")
      return String;
   function Element
     (Path    : String;
      N       : Positive;
      Default : String := "")
      return String;
   -- Returns the N-th element of the Path or the Default if no such element
   -- exists.
   -- For example, the first element of /foo/bar/bla is "foo", the second is
   -- "bar", the third is "bla" and the fourth etc. one is "" unless Default is
   -- set to another value.

private
   Initial : constant Integer := 0;

   type Iterator_Type is
      record
         I : Integer := Initial;
      end record;

   Final : constant Iterator_Type := Iterator_Type'(I => -1);

   pragma Inline (Is_Final);

end REST.Path_Parsers;

