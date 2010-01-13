-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Gen_BTrees.Gen_Stats;

with DB.Utils.Gen_Integer_Image;

procedure DB.Gen_Blob_Trees.Gen_Stats
  (Tree                   : in out Tree_Type;
   Height                 :    out Natural;
   Blocks                 :    out Natural;
   Free_Blocks            :    out Natural;
   Max_Degree             :    out Natural;
   Avg_Degree             :    out Natural;
   Min_Degree             :    out Natural;
   Bytes_Wasted_In_Blocks :    out Long_Integer;
   Bytes_In_Blocks        :    out Long_Integer)
is
   procedure Stats is new BTrees.Gen_Stats;
begin
   Stats(Tree.BTree, Height, Blocks, Free_Blocks, Max_Degree, Avg_Degree,
         Min_Degree, Bytes_Wasted_In_Blocks, Bytes_In_Blocks);
end DB.Gen_Blob_Trees.Gen_Stats;

