-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with System;
with System.Pool_Global; use System.Pool_Global;
with System.Storage_Elements;
with System.Storage_Pools; use System.Storage_Pools;

with This_Computer;

with DB.Gen_Heaps;
with DB.IO.Blocks;
with DB.IO.Blocks.Direct_IO;
with DB.Utils.Traceback;

procedure Heap
is
   File_Name  : constant String  := This_Computer.Heap_File;
   Count      : constant         := 1_00;
   Do_Puts    : constant Boolean := False;
   Do_Gets    : constant Boolean := True;
   Do_Deletes : constant Boolean := False;

   subtype Storage_Element_Type is DB.IO.Blocks.Storage_Element_Type;
   subtype Base_Block_Type is System.Storage_Elements.Storage_Array;
   subtype Size_Type is DB.IO.Blocks.Size_Type;

   type Item_Type is
      record
         I : Size_Type;
      end record;

   function To_Storage_Array (Item : Item_Type) return Base_Block_Type
   is
      use type Storage_Element_Type;
      use type Size_Type;
      Len  : constant Size_Type := Item.I;
      Val  : Size_Type := (Item.I mod Size_Type(Storage_Element_Type'Last)) +
                           Size_Type(Storage_Element_Type'First);
      Byte : Storage_Element_Type := Storage_Element_Type(Val);
      Arr  : Base_Block_Type(1 .. Len) := (others => Byte);
   begin
      return Arr;
   end;

   function From_Storage_Array (Arr : Base_Block_Type) return Item_Type is
   begin
      return (I => Arr'Length);
   end;

   function Info_Index_ID (ID : String) return String is
   begin
      return ID &"_info";
   end;

   function Free_Index_ID (ID : String) return String is
   begin
      return ID &"_free";
   end;

   package Block_IO renames DB.IO.Blocks.Direct_IO.IO;

   package Heaps is new DB.Gen_Heaps
     (Item_Type          => Item_Type,
      To_Storage_Array   => To_Storage_Array,
      From_Storage_Array => From_Storage_Array,
      Info_Index_ID      => Info_Index_ID,
      Free_Index_ID      => Free_Index_ID,
      Storage_Pool       => Root_Storage_Pool'Class(Global_Pool_Object),
      Block_IO           => Block_IO);

   type Address_Array_Type is array (Size_Type range <>) of
      Block_IO.Valid_Address_Type;

   use type DB.IO.Blocks.Size_Type;
   use type Heaps.Result_Type;
   Heap      : Heaps.Heap_Type;
   Addresses : Address_Array_Type(1 .. Count);
begin
   declare
   begin
      Put_Line("Creating "& File_Name);
      Heaps.Create(File_Name);
      Put_Line("Successful");
   exception
      when Error : others =>
         DB.Utils.Traceback.Print_Traceback(Error);
         Put_Line("Failed");
   end;

   Put_Line("Initializing and Finalizing");
   declare
      Tmp_Heap : Heaps.Heap_Type;
   begin
      Heaps.Initialize(Tmp_Heap, File_Name);
      Heaps.Finalize(Tmp_Heap);
   end;
   Put_Line("Successful");

   Put_Line("Initializing");
   Heaps.Initialize(Heap, File_Name);
   Put_Line("Successful");

   Put_Line("PUTS");
   for I in 1 .. Count loop
      declare
         S     : constant Size_Type := Size_Type(I);
         Item  : Item_Type := (I => S);
         State : Heaps.Result_Type;
      begin
         Heaps.Put(Heap, Item, Addresses(S), State);
         if State /= Heaps.Success then
            Put_Line("Put"& Size_Type'Image(S) &" = "&
                     Heaps.Result_Type'Image(State));
         end if;
      end;
   end loop;

   Put_Line("GETS");
   for I in 1 .. Count loop
      declare
         S     : constant Size_Type := Size_Type(I);
         Item  : Item_Type;
         State : Heaps.Result_Type;
      begin
         Heaps.Get(Heap, Addresses(S), Item, State);
         if State /= Heaps.Success then
            Put_Line("Get"& Size_Type'Image(S)&
                     Size_Type'Image(Item.I) &" = "&
                     Heaps.Result_Type'Image(State));
         end if;
      end;
   end loop;

   --New_Line;
   --Put_Line("LULU0");
   --Heaps.Print_Info_Index(Heap);
   --Heaps.Print_Free_Index(Heap);

   -- [0.0, 0.25]
   Put_Line("DELETES [0.0, 0.25)");
   for I in Count*0/4+1 .. Count*1/4 loop
      declare
         S     : constant Size_Type := Size_Type(I);
         State : Heaps.Result_Type := Heaps.Failure;
      begin
         Heaps.Delete(Heap, Addresses(S), State);
         if State /= Heaps.Success then
            Put_Line("Delete"& Size_Type'Image(S) &" = "&
                     Heaps.Result_Type'Image(State));
         end if;
      end;
   end loop;

   --Put_Line("LULU1");
   --Heaps.Print_Info_Index(Heap);
   --Heaps.Print_Free_Index(Heap);

   -- (0.75, 1.0]
   Put_Line("DELETES (0.75, 1.0]");
   for I in Count*3/4+1 .. Count*4/4 loop
      declare
         S     : constant Size_Type := Size_Type(I);
         State : Heaps.Result_Type := Heaps.Failure;
      begin
         Heaps.Delete(Heap, Addresses(S), State);
         if State /= Heaps.Success then
            Put_Line("Delete"& Size_Type'Image(S) &" = "&
                     Heaps.Result_Type'Image(State));
         end if;
      end;
   end loop;

   --Put_Line("LULU2");
   --Heaps.Print_Info_Index(Heap);
   --Heaps.Print_Free_Index(Heap);

   -- (0.25, 0.75], even
   Put_Line("DELETES (0.25, 0.75] even");
   for I in Count*1/4+1 .. Count*3/4 loop
      if I mod 2 = 0 then
         declare
            S     : constant Size_Type := Size_Type(I);
            State : Heaps.Result_Type := Heaps.Failure;
         begin
            Heaps.Delete(Heap, Addresses(S), State);
            if State /= Heaps.Success then
               Put_Line("Delete"& Size_Type'Image(S) &" = "&
                        Heaps.Result_Type'Image(State));
            end if;
         end;
      end if;
   end loop;

   --Put_Line("LULU3");
   --Heaps.Print_Info_Index(Heap);
   --Heaps.Print_Free_Index(Heap);

   -- (0.25, 0.75], odd
   Put_Line("DELETES (0.25, 0.75] odd");
   for I in Count*1/4+1 .. Count*3/4 loop
      if I mod 2 = 1 then
         declare
            S     : constant Size_Type := Size_Type(I);
            State : Heaps.Result_Type := Heaps.Failure;
         begin
            Heaps.Delete(Heap, Addresses(S), State);
            if State /= Heaps.Success then
               Put_Line("Delete"& Size_Type'Image(S) &" = "&
                        Heaps.Result_Type'Image(State));
            end if;
         end;
      end if;
   end loop;

   --Put_Line("LULU4");
   --Heaps.Print_Info_Index(Heap);
   --Heaps.Print_Free_Index(Heap);

   Put_Line("PUTS");
   for I in 1 .. Count loop
      declare
         S     : constant Size_Type := Size_Type(I);
         Item  : Item_Type := (I => S);
         State : Heaps.Result_Type;
      begin
         Heaps.Put(Heap, Item, Addresses(S), State);
         if State /= Heaps.Success then
            Put_Line("Put"& --Size_Type'Image(S) &" = "&
                     Heaps.Result_Type'Image(State));
         end if;
      end;
   end loop;

   Put_Line("GETS");
   for I in 1 .. Count loop
      declare
         S     : constant Size_Type := Size_Type(I);
         Item  : Item_Type;
         State : Heaps.Result_Type;
      begin
         Heaps.Get(Heap, Addresses(S), Item, State);
         if State /= Heaps.Success then
            Put_Line("Get"& Size_Type'Image(S)&
                     Size_Type'Image(Item.I) &" = "&
                     Heaps.Result_Type'Image(State));
         end if;
      end;
   end loop;

   --Put_Line("LULU5");
   --Heaps.Print_Info_Index(Heap);
   --Heaps.Print_Free_Index(Heap);

   Heaps.Finalize(Heap);

exception
   when Error : others =>
      Put_Line("Traceback");
      DB.Utils.Traceback.Print_Traceback(Error);
end Heap;

