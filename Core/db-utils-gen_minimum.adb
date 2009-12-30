-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

function DB.Utils.Gen_Minimum
  (M : Number_Type;
   N : Number_Type)
   return Number_Type is
begin
   return Number_Type'Min(M, N);
end DB.Utils.Gen_Minimum;

