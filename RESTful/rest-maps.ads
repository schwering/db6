with Ada.Containers.Ordered_Maps;
with Ada.Strings.Bounded;

with DB.Maps;

package REST.Maps is

   package Map_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (32);
   subtype Map_Name_Type is Map_Names.Bounded_String;

   subtype Map_Ref_Type is DB.Maps.Map_Ref_Type;

   function Map (Name : Map_Name_Type) return Map_Ref_Type;

private
   Names : constant array (Positive range <>) of Map_Name_Type :=
     (Map_Names.To_Bounded_String ("huhu"),
      Map_Names.To_Bounded_String ("haha"));

   package Map_Maps is new Ada.Containers.Ordered_Maps
     (Map_Name_Type, Map_Ref_Type, Map_Names."<", DB.Maps."=");

    Maps : Map_Maps.Map := Map_Maps.Empty_Map;

end REST.Maps;

