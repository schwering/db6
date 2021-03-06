-- Abstract:
--
-- Administrates the opened maps.
--
-- Copyright 2010--2011 Christoph Schwering

private with Ada.Containers.Ordered_Maps;
private with Ada.Strings;
private with Ada.Strings.Bounded;
private with Ada.Strings.Unbounded;

with DB.Maps;

package REST.Maps is

   subtype Map_Ref_Type is DB.Maps.Map_Ref_Type;

   function Count return Natural;
   function Map_Name (N : Positive) return String;

   function Map_By_Name (Name : String) return Map_Ref_Type;

   function Is_Valid_User (User, Password : String) return Boolean;

private
   use Ada.Strings.Unbounded;

   package Map_Names is new Ada.Strings.Bounded.Generic_Bounded_Length (32);
   subtype Map_Name_Type is Map_Names.Bounded_String;

   package Paths is new Ada.Strings.Bounded.Generic_Bounded_Length (32);
   subtype Path_Type is Paths.Bounded_String;

   function Map_By_Name (Name : Map_Name_Type) return Map_Ref_Type;

   type Map_Info_Type is
      record
         Name : Map_Name_Type;
         Path : Path_Type;
         Impl : DB.Maps.Implementation_Type;
      end record;

   function TB
     (S    : String;
      Drop : Ada.Strings.Truncation := Ada.Strings.Error)
      return Map_Names.Bounded_String
   renames Map_Names.To_Bounded_String;

   function TB
     (S    : String;
      Drop : Ada.Strings.Truncation := Ada.Strings.Error)
      return Paths.Bounded_String
   renames Paths.To_Bounded_String;

   Infos : constant array (Positive range <>) of Map_Info_Type :=
     (1 => (TB ("test"), TB("../Core2/.tmp/btree"), DB.Maps.BTree));

   package Map_Maps is new Ada.Containers.Ordered_Maps
     (Map_Name_Type, Map_Ref_Type, Map_Names."<", DB.Maps."=");

    Initialized : Boolean := False;
    Maps        : Map_Maps.Map := Map_Maps.Empty_Map;

    pragma Volatile (Initialized);

end REST.Maps;

