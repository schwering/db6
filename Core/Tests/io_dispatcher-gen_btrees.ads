with DB.Gen_BTrees;

private
generic
   with package BTrees is new DB.Gen_BTrees (<>);
   with procedure Check (T : in out BTrees.Tree_Type);
   with procedure Stats
          (Tree                   : in out BTrees.Tree_Type;
           Height                 :    out Natural;
           Blocks                 :    out Natural;
           Free_Blocks            :    out Natural;
           Max_Degree             :    out Natural;
           Avg_Degree             :    out Natural;
           Min_Degree             :    out Natural;
           Bytes_Wasted_In_Blocks :    out Long_Integer;
           Bytes_In_Blocks        :    out Long_Integer);
procedure IO_Dispatcher.Gen_BTrees;

