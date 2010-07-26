-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Tags; use Ada.Tags;
with Ada.Exceptions; use Ada.Exceptions;

with AUnit.Assertions; use AUnit.Assertions;

package body DB.Maps.Tag_Map.Test is

   type T1 is tagged null record;
   type T2 is tagged null record;
   type T3 is tagged null record;
   type T4 is tagged null record;
   type Some_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Long_Name is tagged null record;

   procedure Test_Register (T : in out Test_Type)
   is
      pragma Unreferenced (T);
   begin
      Register_Tag (T2'Tag);
      Register_Tag (T3'Tag);
      Register_Tag (T1'Tag);
      Register_Tag (T3'Tag);
      declare
      begin
         Register_Tag (Some_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Long_Name'Tag);
         Assert (False, "too long name accepted");
      exception
         when Overflow_Error => null;
      end;
   end;


   procedure Test_Seal (T : in out Test_Type)
   is
      pragma Unreferenced (T);
   begin
      Seal;
      for TID in Map'First + 3 .. Map'Last loop
         Assert (not Map (Map'Last).Valid, "TID "& TID'Img &" is not invalid");
      end loop;
   end;


   procedure Test_Map (T : in out Test_Type)
   is
      pragma Unreferenced (T);
   begin
      Assert (To_TID (T1'Tag) = Map'First + 0, "TID for T1 is wrong: "&
         To_TID (T1'Tag)'Img);
      Assert (To_TID (T2'Tag) = Map'First + 1, "TID for T2 is wrong: "&
         To_TID (T2'Tag)'Img);
      Assert (To_TID (T3'Tag) = Map'First + 2, "TID for T3 is wrong: "&
         To_TID (T3'Tag)'Img);
      Assert (To_Tag (To_TID (T1'Tag)) = T1'Tag, "inverse of "&
         External_Tag (T1'Tag) &" doesn't match ("& To_TID (T1'Tag)'Img &")");
      Assert (To_Tag (To_TID (T2'Tag)) = T2'Tag, "inverse of "&
         External_Tag (T2'Tag) &" doesn't match ("& To_TID (T2'Tag)'Img &")");
      Assert (To_Tag (To_TID (T3'Tag)) = T3'Tag, "inverse of "&
         External_Tag (T3'Tag) &" doesn't match ("& To_TID (T3'Tag)'Img &")");
      declare
         TID : TID_Type;
      begin
         TID := To_TID (T4'Tag);
         Assert (False, "T4 was not registered but yielded TID "& TID'Img);
      exception
         when Tag_Error => null;
      end;
   end;

end DB.Maps.Tag_Map.Test;

