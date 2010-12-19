-- Abstract:
--
-- Parser for URL paths:
--
-- declare
--    P : constant String := AWS.URL.Pathname (URL);
--    I : Path_Parsers.Iterator := Path_Parsers.Initial;
-- begin
--    while I /= Invalid loop
--       Next ();
--       declare
--          Item : constant String := Path_Parsers.Value (I);
--       begin
--          ...
--       end;
--    end loop;
-- end;
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package REST.Path_Parsers is

   type Iterator_Type is private;

   procedure Next (Path : in String; Iterator : in out Iterator_Type);
   function Value (Path : String; Iterator : Iterator_Type) return String;
   function Is_Final (Iterator : Iterator_Type) return Boolean;

private
   Initial : constant Integer := 0;

   type Iterator_Type is
      record
         I : Integer := Initial;
      end record;

   Final : constant Iterator_Type := Iterator_Type'(I => -1);

   pragma Inline (Is_Final);

end REST.Path_Parsers;

