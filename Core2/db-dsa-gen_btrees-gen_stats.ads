-- Abstract:
--
-- Calculates statistics of a BTree.
-- The keys are: Count, Degree, Size, Waste.
--
-- Copyright 2008--2011 Christoph Schwering

generic
package DB.DSA.Gen_BTrees.Gen_Stats is
   pragma Preelaborate;

   subtype Level_Type is Natural;
   subtype Absolute_Type is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   type Average_Type is delta 10.0**(-1) digits 18;
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
     (Tree : in out Tree_Type;
      Emit : not null access procedure (Level : in Level_Type;
                                        Key   : in String;
                                        Value : in Data_Type));

end DB.DSA.Gen_BTrees.Gen_Stats;

