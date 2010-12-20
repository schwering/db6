with REST.Error_Log;

package body REST.Maps is

   function Map (Name : Map_Name_Type) return Map_Ref_Type is
   begin
      return Maps.Element (Name);
   end;

begin
   for I in Infos'Range loop
      declare
         Map : Map_Ref_Type;
      begin
         Map := DB.Maps.New_Map_Ref (Infos (I).Impl);
         DB.Maps.Open (Map.all, Map_Names.To_String (Infos (I).Name));
         Maps.Insert (Infos (I).Name, Map);
      exception
         when E : others =>
            Error_Log.Log (E);
      end;
   end loop;
end REST.Maps;

