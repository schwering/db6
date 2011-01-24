-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body REST.Maps.Cursors is

   function Bound
     (Row       : String;
      Inclusive : Boolean;
      Lower     : Boolean)
      return DB.Maps.Bound_Type
   is
      function Infinity return DB.Maps.Bound_Type is
      begin
         if Lower then
            return DB.Maps.Negative_Infinity_Bound;
         else
            return DB.Maps.Positive_Infinity_Bound;
         end if;
      end Infinity;

      function Comparison return DB.Maps.Comparison_Type is
      begin
         case Lower is
            when True =>
               case Inclusive is
                  when True  => return DB.Maps.Greater;
                  when False => return DB.Maps.Greater_Or_Equal;
               end case;
            when False =>
               case Inclusive is
                  when True  => return DB.Maps.Less;
                  when False => return DB.Maps.Less_Or_Equal;
               end case;
         end case;
      end Comparison;
   begin
      if Row = Infinity_Row then
         return Infinity;
      else
         -- How do we determine Max? Have a look at this truth table:
         -- Lower Inclusive => Max
         -- True  True      => False
         -- True  False     => True
         -- False True      => True
         -- False False     => False
         return DB.Maps.New_Bound
           (Comparison, Make_Key (Row, Max => Lower /= Inclusive));
      end if;
   end Bound;

end REST.Maps.Cursors;

