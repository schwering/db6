-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;

with Interfaces.C;

with DB.Maps.Tag_Map;
with DB.Maps.Values.Booleans;
with DB.Maps.Values.Floats;
with DB.Maps.Values.Integers;
with DB.Maps.Values.Long_Floats;
with DB.Maps.Values.Long_Integers;
with DB.Maps.Values.Nothings;
with DB.Maps.Values.Strings;

with REST.Log;

package body REST.Maps.Test_Utils is

   procedure Unlink (File_Name : in String)
   is
      use Interfaces.C;

      procedure Unlink (Pathname : char_array);
      pragma Import (C, Unlink, "unlink");

      C_File_Name : constant char_array := To_C (File_Name);
   begin
      Unlink (C_File_Name);
   end Unlink;


   function TS (S : Paths.Bounded_String) return String is
   begin
      return Paths.To_String (S);
   end TS;


   procedure Delete_And_Create_Maps is
   begin
      if not Initialized then
         DB.Maps.Tag_Map.Register (DB.Maps.Values.Booleans.Value_Type'Tag);
         DB.Maps.Tag_Map.Register (DB.Maps.Values.Floats.Value_Type'Tag);
         DB.Maps.Tag_Map.Register (DB.Maps.Values.Integers.Value_Type'Tag);
         DB.Maps.Tag_Map.Register (DB.Maps.Values.Long_Floats.Value_Type'Tag);
         DB.Maps.Tag_Map.Register (DB.Maps.Values.Long_Integers.Value_Type'Tag);
         DB.Maps.Tag_Map.Register (DB.Maps.Values.Nothings.Value_Type'Tag);
         DB.Maps.Tag_Map.Register (DB.Maps.Values.Strings.Value_Type'Tag);
         DB.Maps.Tag_Map.Seal;
      end if;

      Initialized := True;

      for I in Infos'Range loop
         declare
            procedure Free is new Ada.Unchecked_Deallocation
              (DB.Maps.Map_Type'Class, Map_Ref_Type);
            Map : Map_Ref_Type;
         begin
            Unlink (TS (Infos (I).Path));
            if Maps.Contains (Infos (I).Name) then
               Map := Maps.Element (Infos (I).Name);
               Free (Map);
               Maps.Exclude (Infos (I).Name);
            end if;
            Map := DB.Maps.New_Map_Ref (Infos (I).Impl);
            DB.Maps.Create (Map.all, TS (Infos (I).Path));
            Maps.Insert (Infos (I).Name, Map);
         exception
            when E : others =>
               Log.Error (E);
         end;
      end loop;
   end Delete_And_Create_Maps;

end REST.Maps.Test_Utils;

