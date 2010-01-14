-- Abstract:
--
-- Checks a map's BTree for consistency.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

procedure DB.Tables.Maps.Stats
  (Map                    : in out Map_Type;
   Height                 :    out Natural;
   Blocks                 :    out Natural;
   Free_Blocks            :    out Natural;
   Max_Degree             :    out Natural;
   Avg_Degree             :    out Natural;
   Min_Degree             :    out Natural;
   Bytes_Wasted_In_Blocks :    out Long_Integer;
   Bytes_In_Blocks        :    out Long_Integer);
--pragma Preelaborate (DB.Tables.Maps.Check);

