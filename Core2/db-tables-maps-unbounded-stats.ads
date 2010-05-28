-- Abstract:
--
-- Calculates statistics of the map.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package DB.Tables.Maps.Unbounded.Stats is
   --pragma Preelaborate;

   subtype Level_Type is Natural;
   subtype Absolute_Type is Long_Integer;
   type Average_Type is delta 10.0**(-1) digits 17;
   type Data_Type (Compound : Boolean) is
      record
         case Compound is
            when True =>
               Avg : Average_Type;
               Var : Average_Type;
               Min : Absolute_Type;
               Max : Absolute_Type;
            when False =>
               Val : Absolute_Type;
         end case;
      end record;

   procedure Make_Stats
     (Map  : in out Map_Type;
      Emit : not null access procedure (Level : in Level_Type;
                                        Key   : in String;
                                        Value : in Data_Type));

end DB.Tables.Maps.Unbounded.Stats;

