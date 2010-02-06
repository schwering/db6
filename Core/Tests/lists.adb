with Ada.Text_IO; use Ada.Text_IO;
with DB.Utils.Gen_Linked_Lists;
with DB.Utils.Global_Pool;
with System.Storage_Pools; use System.Storage_Pools;

procedure Lists is
   subtype Item_Type is Natural;

   package Lists is new DB.Utils.Gen_Linked_Lists
      (Item_Type,
       "=",
       Root_Storage_Pool'Class(DB.Utils.Global_Pool.Global'Storage_Pool));
   
   List : Lists.List_Type;
begin
   pragma Assert (Lists.Is_Empty(List) and Lists.Size(List) = 0);
   for I in 1 .. 1000 loop
      if I mod 2 = 0 then
         Lists.Append(List, I);
      end if;
   end loop;
   pragma Assert (not Lists.Is_Empty(List) and Lists.Size(List) = 500);
   declare
      Iter    : Lists.Iterator_Type := Lists.Iterator(List);
      I       : Natural := 1;
      Item    : Item_Type;
   begin
      while Lists.Has_Next(Iter) loop
         Lists.Get_Next(Iter, Item);
         pragma Assert (Item = 2*I);
         I := I + 1;
      end loop;
      pragma Assert (I-1 = 500);
   end;

   for I in reverse 1 .. 1000 loop
      if I mod 2 /= 0 then
         Lists.Prepend(List, I);
      end if;
   end loop;
   pragma Assert (not Lists.Is_Empty(List) and Lists.Size(List) = 1000);
   declare
      Iter    : Lists.Iterator_Type := Lists.Iterator(List);
      I       : Natural := 1;
      Item    : Item_Type;
   begin
      while Lists.Has_Next(Iter) loop
         Lists.Get_Next(Iter, Item);
         pragma Assert (Item = 2*I-1 or I > 500);
         I := I + 1;
      end loop;
      pragma Assert (I-1 = 1000);
   end;

   declare
      Iter    : Lists.Iterator_Type := Lists.Iterator(List);
      I       : Natural := 1;
      Item    : Item_Type;
   begin
      while Lists.Has_Next(Iter) loop
         Lists.Get_Next(Iter, Item);
         if I <= 500 and Item /= 2*I-1 then
            Put_Line("Fehler bei Element"& I'Img &":"& Item'Img &" statt "&
                     Natural'Image(2*I-1));
         elsif I > 500 and Item /= 2*(I-500) then
            Put_Line("Fehler bei Element"& I'Img &":"& Item'Img &" statt "&
                     Natural'Image(2*(I-500)));
         end if;
         I := I + 1;
      end loop;
      if (I-1) /= 1000 then
         Put_Line("Statt 1000 sind"& I'Img &" Elemente drin");
      end if;
   end;


   for I in reverse 1 .. 1000 loop
      if I mod 2 /= 0 then
         Lists.Prepend(List, I);
      end if;
   end loop;
   pragma Assert (not Lists.Is_Empty(List) and Lists.Size(List) = 1500);
   declare
      Iter    : Lists.Iterator_Type := Lists.Iterator(List);
      I       : Natural := 1;
      Item    : Item_Type;
   begin
      while Lists.Has_Next(Iter) loop
         Lists.Get_Next(Iter, Item);
         pragma Assert (Item = 2*I-1 or I > 500);
         Lists.Delete(Iter);
         I := I + 1;
      end loop;
      pragma Assert (I-1 = 1500);
      pragma Assert (Lists.Is_Empty(List) and Lists.Size(List) = 0);
   end;

   Put_Line("Alles ok");
end Lists;

