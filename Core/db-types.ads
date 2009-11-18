-- Abstract:
--
-- Root package for data types.
--
-- Copyright 2008, 2009 Christoph Schwering

package DB.Types is
   pragma Pure;

   type Letter_Type is
      ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
       'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
       '-', '_', '.', ':', '/', '&');

   type Letter_Array_Type is array (Positive range <>) of Letter_Type;
   pragma Pack (Letter_Array_Type);

   type Time_Type is mod 2**64;

   function Image (L : Letter_Type) return String;
   function Image (C : Character) return String;

private
   pragma Inline (Image);

end DB.Types;

