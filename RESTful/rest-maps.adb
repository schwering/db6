-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with DB.Maps.Tag_Map;
with DB.Maps.Values.Booleans;
with DB.Maps.Values.Floats;
with DB.Maps.Values.Integers;
with DB.Maps.Values.Long_Floats;
with DB.Maps.Values.Long_Integers;
with DB.Maps.Values.Nothings;
with DB.Maps.Values.Strings;

with REST.Log;

package body REST.Maps is

   function TS (S : Map_Names.Bounded_String) return String is
   begin
      return Map_Names.To_String (S);
   end TS;


   function TS (S : Paths.Bounded_String) return String is
   begin
      return Paths.To_String (S);
   end TS;


   procedure Init is
   begin
      if Initialized then
         return;
      end if;

      Initialized := True;

      DB.Maps.Tag_Map.Register (DB.Maps.Values.Booleans.Value_Type'Tag);
      DB.Maps.Tag_Map.Register (DB.Maps.Values.Floats.Value_Type'Tag);
      DB.Maps.Tag_Map.Register (DB.Maps.Values.Integers.Value_Type'Tag);
      DB.Maps.Tag_Map.Register (DB.Maps.Values.Long_Floats.Value_Type'Tag);
      DB.Maps.Tag_Map.Register (DB.Maps.Values.Long_Integers.Value_Type'Tag);
      DB.Maps.Tag_Map.Register (DB.Maps.Values.Nothings.Value_Type'Tag);
      DB.Maps.Tag_Map.Register (DB.Maps.Values.Strings.Value_Type'Tag);
      DB.Maps.Tag_Map.Seal;

      for I in Infos'Range loop
         declare
            Map : Map_Ref_Type;
         begin
            Map := DB.Maps.New_Map_Ref (Infos (I).Impl);
            DB.Maps.Open (Map.all, TS (Infos (I).Path));
            Maps.Insert (Infos (I).Name, Map);
         exception
            when E : others =>
               Log.Error (E);
         end;
      end loop;
   end Init;


   function Map_By_Name (Name : String) return Map_Ref_Type is
   begin
      Init;
      return Map_By_Name (TB (Name));
   end Map_By_Name;


   function Map_By_Name (Name : Map_Name_Type) return Map_Ref_Type is
   begin
      Init;
      return Maps.Element (Name);
   end Map_By_Name;

end REST.Maps;

