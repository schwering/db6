with DB.Types.Gen_Bounded_Strings;

package DB.Types.Rows is new Types.Gen_Bounded_Strings
  (Item_Type  => Character,
   Max_Length => 2048);
pragma Preelaborate (DB.Types.Rows);

