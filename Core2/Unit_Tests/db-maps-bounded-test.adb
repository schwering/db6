-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

with AUnit.Assertions; use AUnit.Assertions;

with DB.Maps.Test_Utils;
with DB.Types.Keys; use DB.Types.Keys;

package body DB.Maps.Bounded.Test is

   File_Name : constant String := "unit_test_bounded_map";

   procedure Set_Up (T : in out Test_Type) is
   begin
      T.Map := new Map_Type'(New_Map (Default_Allow_Duplicates));
      Create (T.Map.all, File_Name);
   end;


   procedure Tear_Down (T : in out Test_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation (Map_Type, Map_Ref_Type);
   begin
      Finalize (T.Map.all);
      Free (T.Map);
      Test_Utils.Unlink (File_Name);
   end;


   function New_Key (Row, Col : String; Time : Natural) return Key_Type is
   begin
      return Key_Type'(Rows.New_String (Rows.Indefinite_Buffer_Type (Row)),
                       Columns.New_String (Rows.Indefinite_Buffer_Type (Col)),
                       Times.Number_Type (Time));
   end New_Key;

end DB.Maps.Bounded.Test;

