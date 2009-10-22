-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with System.Pool_Global; use System.Pool_Global;
with System.Storage_Pools; use System.Storage_Pools;

with DB.Utils;
with DB.Utils.Gen_HashTables;

procedure Hash
is
   use DB.Utils;
   Size : constant := 10_000;
   subtype Key_Type is Integer;
   subtype Value_Type is Integer;

   function Hash (K : Key_Type) return Hash_Type
   is begin
      return Hash_Type(3 * K + 1);
   end Hash;

   function Rehash (H : Hash_Type) return Hash_Type
   is begin
      return H + 1;
   end Rehash;

   package HT is new DB.Utils.Gen_Hashtables
     (Key_Type     => Key_Type,
      Value_Type   => Value_Type,
      Hash         => Hash,
      Rehash       => Rehash,
      Storage_Pool => Root_Storage_Pool'Class(Global_Pool_Object));

   subtype Test_Range is Integer range 1 .. Size*3/2;

   T : HT.Table_Type := HT.New_Table(Size * 3 / 2);
begin
   for I in Test_Range loop
      if HT.Contains(T, I) then
         Put_Line("1.1 "& Integer'Image(I));
      end if;
   end loop;

   for I in Test_Range loop
      HT.Put(T, I, I);
      if not HT.Contains(T, I) then
         Put_Line("2.1 "& Integer'Image(I));
      end if;
   end loop;

   for I in Test_Range loop
      HT.Put(T, I, I);
      if not HT.Contains(T, I) then
         Put_Line("2.1 "& Integer'Image(I));
      end if;
   end loop;

   for I in Test_Range loop
      declare
         K : constant Key_Type := I;
         V : Value_Type;
         F : Boolean;
      begin
         HT.Get(T, K, V, F);
         if not F then
            Put_Line("3.1 "& Integer'Image(I));
         end if;
         if K /= V then
            Put_Line("3.2 "& Key_Type'Image(K) & Value_Type'Image(V));
         end if;
      end;
   end loop;

   for I in Test_Range loop
      if not HT.Contains(T, I) then
         Put_Line("4.1 "& Integer'Image(I));
      end if;
      HT.Delete(T, I);
      if HT.Contains(T, I) then
         Put_Line("4.2 "& Integer'Image(I));
      end if;
   end loop;


   for I in Test_Range loop
      HT.Put(T, I, I);
      if not HT.Contains(T, I) then
         Put_Line("5.1 "& Integer'Image(I));
      end if;
   end loop;

   for I in Test_Range loop
      HT.Put(T, I, I);
      if not HT.Contains(T, I) then
         Put_Line("2.1 "& Integer'Image(I));
      end if;
   end loop;

   for I in Test_Range loop
      declare
         K : constant Key_Type := I;
         V : Value_Type;
         F : Boolean;
      begin
         HT.Get(T, K, V, F);
         if not F then
            Put_Line("3.1 "& Integer'Image(I));
         end if;
         if K /= V then
            Put_Line("3.2 "& Key_Type'Image(K) & Value_Type'Image(V));
         end if;
      end;
   end loop;

   for I in Test_Range loop
      if not HT.Contains(T, I) then
         Put_Line("4.1 "& Integer'Image(I));
      end if;
      HT.Delete(T, I);
      if HT.Contains(T, I) then
         Put_Line("4.2 "& Integer'Image(I));
      end if;
   end loop;


   for I in Test_Range loop
      HT.Put(T, I, I);
      if not HT.Contains(T, I) then
         Put_Line("5.1 "& Integer'Image(I));
      end if;
   end loop;

   for I in Test_Range loop
      declare
         K : constant Key_Type := I;
         V : Value_Type;
         F : Boolean;
      begin
         HT.Get(T, K, V, F);
         if not F then
            Put_Line("6.1 "& Integer'Image(I));
         end if;
         if K /= V then
            Put_Line("6.2 "& Key_Type'Image(K) & Value_Type'Image(V));
         end if;
      end;
   end loop;

   for I in Test_Range loop
      if not HT.Contains(T, I) then
         Put_Line("7.1 "& Integer'Image(I));
      end if;
      HT.Delete(T, I);
      if HT.Contains(T, I) then
         Put_Line("7.2 "& Integer'Image(I));
      end if;
   end loop;

end Hash;

