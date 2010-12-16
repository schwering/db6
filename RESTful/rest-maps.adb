with REST.Error_Log;

package body REST.Maps is

   function Map (Name : Map_Name_Type) return Map_Ref_Type is
   begin
      return Maps.Element (Name);
   end;

begin
   for I in Names'Range loop
      declare
         Map : Map_Ref_Type;
      begin
         Map := DB.Maps.New_Map_Ref ("covered");
         DB.Maps.Open (Map.all, Map_Names.To_String (Names (I)));
         Maps.Insert (Names (I), Map);
      exception
         when E : others =>
            Error_Log.Push (E);
      end;
   end loop;
end REST.Maps;

