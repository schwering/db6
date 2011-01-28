-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

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


   function Count return Natural is
   begin
      return Infos'Length;
   end Count;


   function Map_Name (N : Positive) return String is
   begin
      return Map_Names.To_String (Infos (N).Name);
   end Map_Name;


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


   package User_Maps is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Unbounded_String);

   Users : User_Maps.Map;

   function Is_Valid_User (User, Password : String) return Boolean
   is
      use type User_Maps.Cursor;
      C : constant User_Maps.Cursor :=
        User_Maps.Find (Users, To_Unbounded_String (User));
   begin
      return C /= User_Maps.No_Element and then
             To_String (User_Maps.Element (C)) = Password;
   end Is_Valid_User;

begin
   User_Maps.Insert
     (Users, To_Unbounded_String ("chs"), To_Unbounded_String ("bla")); 
end REST.Maps;

