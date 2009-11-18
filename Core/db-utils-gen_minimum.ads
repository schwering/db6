-- Abstract:
--
-- Simple minimum.
--
-- Copyright 2008, 2009 Christoph Schwering

generic
   type Number_Type is (<>);
function DB.Utils.Gen_Minimum
  (M : Number_Type;
   N : Number_Type)
   return Number_Type;
pragma Pure (DB.Utils.Gen_Minimum);

