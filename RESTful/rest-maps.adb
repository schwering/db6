with REST.Error_Log;

package body REST.Maps is

   function TS (S : Map_Names.Bounded_String) return String is
   begin
      return Map_Names.To_String (S);
   end TS;


   function TS (S : Paths.Bounded_String) return String is
   begin
      return Paths.To_String (S);
   end TS;


   function Map_By_Name (Name : String) return Map_Ref_Type is
   begin
      return Map_By_Name (TB (Name));
   end Map_By_Name;


   function Map_By_Name (Name : Map_Name_Type) return Map_Ref_Type is
   begin
      return Maps.Element (Name);
   end Map_By_Name;

begin
   for I in Infos'Range loop
      declare
         Map : Map_Ref_Type;
      begin
         Map := DB.Maps.New_Map_Ref (Infos (I).Impl);
         DB.Maps.Open (Map.all, TS (Infos (I).Path));
         Maps.Insert (Infos (I).Name, Map);
      exception
         when E : others =>
            Error_Log.Log (E);
      end;
   end loop;
end REST.Maps;

