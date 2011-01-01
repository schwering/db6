-- Abstract:
--
-- see spec
--
-- Design Notes:
--
-- Set_Up and Tear_Down store/restore the state of the tag map before the test
-- ran.
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Tags; use Ada.Tags;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;

package body DB.Maps.Tag_Map.Test is

   type T1 is tagged null record;
   type T2 is tagged null record;
   type T3 is tagged null record;
   type T4 is tagged null record;
   type Some_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Long_Name is tagged null record;


   procedure Set_Up (T : in out Test_Type) is
   begin
      Utils.Store (T.State);
   end;


   procedure Tear_Down (T : in out Test_Type) is
   begin
      Utils.Restore (T.State);
   end;


   procedure Test_Register (T : in out Test_Type'Class)
   is
      pragma Unreferenced (T);
   begin
      Register (T2'Tag);
      Register (T3'Tag);
      Register (T1'Tag);
      Register (T3'Tag);
      declare
      begin
         Register (Some_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Very_Long_Name'Tag);
         Assert (False, "too long name accepted");
      exception
         when Overflow_Error => null;
      end;
   end;


   procedure Test_Seal (T : in out Test_Type'Class)
   is
      pragma Unreferenced (T);
   begin
      Seal;
      for Tid in Map'First + 3 .. Map'Last loop
         Assert (not Map (Map'Last).Valid, "Tid "& Tid'Img &" is not invalid");
      end loop;
   end;


   procedure Test_Map (T : in out Test_Type'Class)
   is
      pragma Unreferenced (T);
   begin
      Assert (To_Tid (T1'Tag) = Map'First + 0, "Tid for T1 is wrong: "&
         To_Tid (T1'Tag)'Img);
      Assert (To_Tid (T2'Tag) = Map'First + 1, "Tid for T2 is wrong: "&
         To_Tid (T2'Tag)'Img);
      Assert (To_Tid (T3'Tag) = Map'First + 2, "Tid for T3 is wrong: "&
         To_Tid (T3'Tag)'Img);
      Assert (To_Tag (To_Tid (T1'Tag)) = T1'Tag, "inverse of "&
         External_Tag (T1'Tag) &" doesn't match ("& To_Tid (T1'Tag)'Img &")");
      Assert (To_Tag (To_Tid (T2'Tag)) = T2'Tag, "inverse of "&
         External_Tag (T2'Tag) &" doesn't match ("& To_Tid (T2'Tag)'Img &")");
      Assert (To_Tag (To_Tid (T3'Tag)) = T3'Tag, "inverse of "&
         External_Tag (T3'Tag) &" doesn't match ("& To_Tid (T3'Tag)'Img &")");
      declare
         Tid : Tid_Type;
      begin
         Tid := To_Tid (T4'Tag);
         Assert (False, "T4 was not registered but yielded Tid "& Tid'Img);
      exception
         when Tag_Error => null;
      end;
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
   end;


   procedure Tests (T : in out Test_Type) is
   begin
      Test_Register (T);
      Test_Seal (T);
      Test_Map (T);
   end Tests;

end DB.Maps.Tag_Map.Test;

