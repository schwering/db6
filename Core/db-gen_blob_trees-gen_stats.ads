-- Abstract:
--
-- Calculates statistics of a Blob-Tree.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

generic
procedure DB.Gen_Blob_Trees.Gen_Stats
  (Tree                   : in out Tree_Type;
   Height                 :    out Natural;
   Blocks                 :    out Natural;
   Free_Blocks            :    out Natural;
   Max_Degree             :    out Natural;
   Avg_Degree             :    out Natural;
   Min_Degree             :    out Natural;
   Bytes_Wasted_In_Blocks :    out Long_Integer;
   Bytes_In_Blocks        :    out Long_Integer);
--pragma Preelaborate (DB.Tables.Maps.Gen_Stats);

