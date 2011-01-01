-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Utils.Growing_Arrays is

   generic
      type Index_Type is range <>;
   function Gen_New_From_Index (I : Index_Type) return Index_Type;

   generic
      type Index_Type is range <>;
   function Gen_New_To_Index (I : Index_Type) return Index_Type;


   function Gen_New_From_Index (I : Index_Type) return Index_Type is
   begin
      if I < 0 then
         return I - 1;
         --return I * 3 / 2 - 10;
      else
         return I - 1;
         --return I * 2 / 3 - 10;
      end if;
   end Gen_New_From_Index;


   function Gen_New_To_Index (I : Index_Type) return Index_Type is
   begin
      if I < 0 then
         return I + 1;
         --return I * 2 / 3 + 10;
      else
         return I + 1;
         --return I * 3 / 2 + 10;
      end if;
   end Gen_New_To_Index;


   procedure Gen_Grow
     (Arr   : in out Array_Access_Type;
      Index : in     Index_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Array_Type, Array_Access_Type);

      procedure Grow (From, To : Index_Type'Base)
      is
         Tmp : Array_Access_Type;
      begin
         pragma Assert (From <= Arr'First);
         pragma Assert (To   >= Arr'Last);

         Tmp := new Array_Type (From .. To);
         Tmp.all := (others => Default_Item);
         Tmp (Arr'Range) := Arr.all;
         Free (Arr);
         Arr := Tmp;
      end Grow;

      function New_From_Index is new Gen_New_From_Index (Index_Type);
      function New_To_Index is new Gen_New_To_Index (Index_Type);
   begin
      if Index < Arr'First then
         Grow (New_From_Index (Index), Arr'Last);
      elsif Index > Arr'Last then
         Grow (Arr'First, New_To_Index (Index));
      end if;
   end Gen_Grow;


   procedure Gen_Grow_2d
     (Arr    : in out Array_Access_Type;
      Index1 : in     First_Index_Type;
      Index2 : in     Second_Index_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Array_Type, Array_Access_Type);

      procedure Grow (From1, To1 : First_Index_Type'Base;
                      From2, To2 : Second_Index_Type'Base)
      is
         Tmp : Array_Access_Type;
      begin
         pragma Assert (From1 <= Arr'First (1));
         pragma Assert (To1   >= Arr'Last (1));
         pragma Assert (From2 <= Arr'First (2));
         pragma Assert (To2   >= Arr'Last (2));

         Tmp := new Array_Type (From1 .. To1, From2 .. To2);
         Tmp.all := (others => (others => Default_Item));
         for I in Arr'Range (1) loop
            for J in Arr'Range (2) loop
               Tmp (I, J) := Arr (I, J);
            end loop;
         end loop;
         Free (Arr);
         Arr := Tmp;

         pragma Assert (Index1 in Arr'Range (1));
         pragma Assert (Index2 in Arr'Range (2));
      end Grow;

      function New_From_Index1 is new Gen_New_From_Index (First_Index_Type);
      function New_To_Index1 is new Gen_New_To_Index (First_Index_Type);
      function New_From_Index2 is new Gen_New_From_Index (Second_Index_Type);
      function New_To_Index2 is new Gen_New_To_Index (Second_Index_Type);
   begin
      if Index1 < Arr'First (1) and Index2 < Arr'First (2) then
         Grow (New_From_Index1 (Index1), Arr'Last (1),
               New_From_Index2 (Index2), Arr'Last (2));
      elsif Index1 < Arr'First (1) and Index2 > Arr'Last (2) then
         Grow (New_From_Index1 (Index1), Arr'Last (1),
               Arr'First (2), New_To_Index2 (Index2));
      elsif Index1 > Arr'Last (1) and Index2 < Arr'First (2) then
         Grow (Arr'First (1), New_To_Index1 (Index1),
               New_From_Index2 (Index2), Arr'Last (2));
      elsif Index1 > Arr'Last (1) and Index2 > Arr'Last (2) then
         Grow (Arr'First (1), New_To_Index1 (Index1),
               Arr'First (2), New_To_Index2 (Index2));
      elsif Index1 < Arr'First (1) then
         Grow (New_From_Index1 (Index1), Arr'Last (1),
               Arr'First (2), Arr'Last (2));
      elsif Index1 > Arr'Last (1) then
         Grow (Arr'First (1), New_To_Index1 (Index1),
               Arr'First (2), Arr'Last (2));
      elsif Index2 < Arr'First (2) then
         Grow (Arr'First (1), Arr'Last (1),
               New_From_Index2 (Index2), Arr'Last (2));
      elsif Index2 > Arr'Last (2) then
         Grow (Arr'First (1), Arr'Last (1),
               Arr'First (2), New_To_Index2 (Index2));
      end if;

      pragma Assert (Index1 in Arr'Range (1));
      pragma Assert (Index2 in Arr'Range (2));
   end Gen_Grow_2d;

end DB.Utils.Growing_Arrays;

