with Ada.Containers.Ordered_Maps;
with Ada.Strings.Bounded;

with DB.Maps;

package REST.Maps is

   subtype Map_Ref_Type is DB.Maps.Map_Ref_Type;

   function Map_By_Name (Name : String) return Map_Ref_Type;

private
   package Map_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (32);
   subtype Map_Name_Type is Map_Names.Bounded_String;

   function Map_By_Name (Name : Map_Name_Type) return Map_Ref_Type;

   type Map_Info_Type is
      record
         Name : Map_Name_Type;
         Impl : DB.Maps.Implementation_Type;
      end record;

   Infos : constant array (Positive range <>) of Map_Info_Type :=
     (1 => Map_Info_Type'(Map_Names.To_Bounded_String ("../Core2/.tmp/btree"),
                          DB.Maps.BTree));

   package Map_Maps is new Ada.Containers.Ordered_Maps
     (Map_Name_Type, Map_Ref_Type, Map_Names."<", DB.Maps."=");

    Maps : Map_Maps.Map := Map_Maps.Empty_Map;

end REST.Maps;

