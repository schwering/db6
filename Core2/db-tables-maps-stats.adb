-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Gen_BTrees.Gen_Stats;
--with DB.Gen_Blob_Trees.Gen_Stats;

package body DB.Tables.Maps.Stats is

   procedure Make_Stats
     (Map  : in out Map_Type;
      Emit : not null access procedure (Level : in Level_Type;
                                        Key   : in String;
                                        Value : in Data_Type)) is
   begin
      case Map.Short is
         when True =>
            declare
               package Stats is new BTrees.Gen_Stats;
               procedure My_Emit
                 (Level : in Stats.Level_Type;
                  Key   : in String;
                  Value : in Stats.Data_Type) is
               begin
                  case Value.Compound is
                     when True =>
                        Emit(Level_Type(Level),
                             Key,
                             Data_Type'(Compound => True,
                                        Avg => Average_Type(Value.Avg),
                                        Min => Absolute_Type(Value.Min),
                                        Max => Absolute_Type(Value.Max)));
                     when False =>
                        Emit(Level_Type(Level),
                             Key,
                             Data_Type'(Compound => False,
                                        Val => Absolute_Type(Value.Val)));
                  end case;
               end;
            begin
               Stats.Make_Stats(Map.Short_Tree, My_Emit'Access);
            end;
         when False =>
            declare
               package Stats is new Blob_Trees.Gen_Stats; -- XXX remove
               procedure My_Emit
                 (Level : in Stats.Level_Type;
                  Key   : in String;
                  Value : in Stats.Data_Type) is
               begin
                  case Value.Compound is
                     when True =>
                        Emit(Level_Type(Level),
                             Key,
                             Data_Type'(Compound => True,
                                        Avg => Average_Type(Value.Avg),
                                        Min => Absolute_Type(Value.Min),
                                        Max => Absolute_Type(Value.Max)));
                     when False =>
                        Emit(Level_Type(Level),
                             Key,
                             Data_Type'(Compound => False,
                                        Val => Absolute_Type(Value.Val)));
                  end case;
               end;
            begin
               Stats.Make_Stats(Map.Long_Tree, My_Emit'Access);
            end;
      end case;
   end Make_Stats;

end DB.Tables.Maps.Stats;
