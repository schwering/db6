-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Gen_BTrees.Gen_Stats;
with DB.Gen_Blob_Trees.Gen_Stats;

with DB.Utils.Gen_Integer_Image;

procedure DB.Tables.Maps.Stats
  (Map                    : in out Map_Type;
   Height                 :    out Natural;
   Blocks                 :    out Natural;
   Free_Blocks            :    out Natural;
   Max_Degree             :    out Natural;
   Avg_Degree             :    out Natural;
   Min_Degree             :    out Natural;
   Bytes_Wasted_In_Blocks :    out Long_Integer;
   Bytes_In_Blocks        :    out Long_Integer) is
begin
   case Map.Short is
      when True =>
         declare
            procedure Stats is new BTrees.Gen_Stats;
         begin
            Stats(Map.Short_Tree, Height, Blocks, Free_Blocks, Max_Degree,
                  Avg_Degree, Min_Degree, Bytes_Wasted_In_Blocks,
                  Bytes_In_Blocks);
         end;
      when False =>
         declare
            procedure Stats is new Blob_Trees.Gen_Stats;
         begin
            Stats(Map.Long_Tree, Height, Blocks, Free_Blocks, Max_Degree,
                  Avg_Degree, Min_Degree, Bytes_Wasted_In_Blocks,
                  Bytes_In_Blocks);
         end;
   end case;
end DB.Tables.Maps.Stats;

