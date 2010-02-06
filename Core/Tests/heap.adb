with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with This_Computer;

with DB.Gen_Heaps;
with DB.IO.Blocks;
with DB.IO.Blocks.Direct_IO;
with DB.Types.Strings;
with DB.Types.Strings.Unbounded;

procedure Heap
is
   File_Name  : constant String  := This_Computer.Heap_File;
   Count      : constant         := 1_000;
   Do_Puts    : constant Boolean := False;
   Do_Gets    : constant Boolean := True;
   Do_Deletes : constant Boolean := False;

   function Info_Index_ID (ID : String) return String is
   begin
      return ID &"_info";
   end;

   function Free_Index_ID (ID : String) return String is
   begin
      return ID &"_free";
   end;

   package Items    renames DB.Types.Strings.Unbounded;
   package Item_IO  renames DB.Types.Strings.Unbounded.Parted;
   package Block_IO renames DB.IO.Blocks.Direct_IO;

   subtype Buffer_Type is DB.Types.Strings.Indefinite_Buffer_Type;
   subtype Length_Type is DB.Types.Strings.Length_Type;
   subtype Index_Type is DB.Types.Strings.Index_Type;
   subtype Item_Type is Items.String_Type;
   use type Length_Type;
   use type Item_Type;

   package Heaps is new DB.Gen_Heaps
     (Item_Type          => Item_Type,
      Item_Context_Type  => Item_IO.Context_Type,
      New_Item_Context   => Item_IO.New_Context,
      Item_Size_Bound    => Item_IO.Size_Bound,
      Fold_Contexts      => Item_IO.Fold_Contexts,
      Context_Size_Bound => Item_IO.Context_Size_Bound,
      Read_Context       => Item_IO.Read_Context,
      Write_Context      => Item_IO.Write_Context,
      Read_Part_Of_Item  => Item_IO.Read_Part_Of_String,
      Write_Part_Of_Item => Item_IO.Write_Part_Of_String,
      Info_Index_ID      => Info_Index_ID,
      Free_Index_ID      => Free_Index_ID,
      Block_IO           => Block_IO.IO,
      IO_Buffers         => Block_IO.IO_Buffers);

   function Make_Item (I : Positive) return Item_Type
   is
      Image  : constant String      := I'Img;
      Count  : constant Length_Type := I;
      Length : constant Length_Type := Count * Image'Length;
      Buf    : Buffer_Type(1 .. Length);
   begin
      for I in 1 .. Count loop
         declare
            From : constant Index_Type := (I-1) * Image'Length + 1;
            To   : constant Index_Type := I * Image'Length;
         begin
            Buf(From .. To) := Buffer_Type(Image);
         end;
      end loop;
      return Items.New_String(Buf);
   end Make_Item;

   function To_String (Item : Item_Type) return String is
   begin
      return String(Items.To_Buffer(Item));
   end To_String;

   type Address_Array_Type is array (Positive range <>) of Heaps.Address_Type;

   use type Heaps.State_Type;
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
         Put_Line("Exception: "& Exception_Information(Error));
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
         Item  : constant Item_Type := Make_Item(I);
         State : Heaps.State_Type;
      begin
         Heaps.Put(Heap, Item, Addresses(I), State);
         if State /= Heaps.Success then
            Put_Line("Put"& I'Img &" = "& State'Img);
         end if;
      end;
   end loop;

   Put_Line("GETS");
   for I in 1 .. Count loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Success or else Item /= Make_Item(I) then
            Put_Line("Get"& I'Img & To_String(Item) &" = "& State'Img);
         end if;
      end;
   end loop;

   Put_Line("APPEND");
   for I in 1 .. Count loop
      declare
         Item  : constant Item_Type := Make_Item(I);
         State : Heaps.State_Type;
      begin
         Heaps.Append(Heap, Item, Addresses(I), State);
         if State /= Heaps.Success then
            Put_Line("Append"& I'Img &" = "& State'Img);
         end if;
      end;
   end loop;

   Put_Line("GETS");
   for I in 1 .. Count loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Success or else
            Item /= Make_Item(I) & Make_Item(I) then
            Put_Line("Get"& I'Img & To_String(Item) &" = "& State'Img);
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
         State : Heaps.State_Type := Heaps.Failure;
      begin
         Heaps.Delete(Heap, Addresses(I), State);
         if State /= Heaps.Success then
            Put_Line("Delete"& I'Img &" = "& State'Img);
         end if;
      end;
   end loop;

   Put_Line("GETS");
   for I in Count*0/4+1 .. Count*1/4 loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Failure then
            Put_Line("Get"& I'Img &" = "& State'Img);
         end if;
      end;
   end loop;
   for I in Count*1/4+1 .. Count loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Success or else
            Item /= Make_Item(I) & Make_Item(I) then
            Put_Line("Get"& I'Img & To_String(Item) &" = "& State'Img);
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
         State : Heaps.State_Type := Heaps.Failure;
      begin
         Heaps.Delete(Heap, Addresses(I), State);
         if State /= Heaps.Success then
            Put_Line("Delete"& I'Img &" = "& State'Img);
         end if;
      end;
   end loop;

   Put_Line("GETS");
   for I in Count*0/4+1 .. Count*1/4 loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Failure then
            Put_Line("Get"& I'Img &" = "& State'Img);
         end if;
      end;
   end loop;
   for I in Count*1/4+1 .. Count*3/4 loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Success or else
            Item /= Make_Item(I) & Make_Item(I) then
            Put_Line("Get"& I'Img & To_String(Item) &" = "& State'Img);
         end if;
      end;
   end loop;
   for I in Count*3/4+1 .. Count*4/4 loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Failure then
            Put_Line("Get"& I'Img &" = "& State'Img);
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
            State : Heaps.State_Type := Heaps.Failure;
         begin
            Heaps.Delete(Heap, Addresses(I), State);
            if State /= Heaps.Success then
               Put_Line("Delete"& I'Img &" = "& State'Img);
            end if;
         end;
      end if;
   end loop;

   Put_Line("GETS");
   for I in Count*0/4+1 .. Count*1/4 loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Failure then
            Put_Line("Get"& I'Img &" = "& State'Img);
         end if;
      end;
   end loop;
   for I in Count*1/4+1 .. Count*3/4 loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if I mod 2 = 0 then
            if State /= Heaps.Failure then
               Put_Line("Get"& I'Img &" = "& State'Img);
            end if;
         else
            if State /= Heaps.Success or else
               Item /= Make_Item(I) & Make_Item(I) then
               Put_Line("Get"& I'Img & To_String(Item) &" = "& State'Img);
            end if;
         end if;
      end;
   end loop;
   for I in Count*3/4+1 .. Count*4/4 loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Failure then
            Put_Line("Get"& I'Img &" = "& State'Img);
         end if;
      end;
   end loop;

   --Put_Line("LULU3");
   --Heaps.Print_Info_Index(Heap);
   --Heaps.Print_Free_Index(Heap);

   -- (0.25, 0.75], odd
   Put_Line("DELETES (0.25, 0.75] odd");
   for I in Count*1/4+1 .. Count*3/4 loop
      if I mod 2 = 1 then
         declare
            State : Heaps.State_Type := Heaps.Failure;
         begin
            Heaps.Delete(Heap, Addresses(I), State);
            if State /= Heaps.Success then
               Put_Line("Delete"& I'Img &" = "& State'Img);
            end if;
         end;
      end if;
   end loop;

   Put_Line("GETS");
   for I in 1 .. Count loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Failure then
            Put_Line("Get"& I'Img &" = "& State'Img);
         end if;
      end;
   end loop;

   --Put_Line("LULU4");
   --Heaps.Print_Info_Index(Heap);
   --Heaps.Print_Free_Index(Heap);

   Put_Line("PUTS");
   for I in 1 .. Count loop
      declare
         Item  : constant Item_Type := Make_Item(I);
         State : Heaps.State_Type;
      begin
         Heaps.Put(Heap, Item, Addresses(I), State);
         if State /= Heaps.Success then
            Put_Line("Put"& I'Img &" = "& State'Img);
         end if;
      end;
   end loop;

   Put_Line("GETS");
   for I in 1 .. Count loop
      declare
         Item  : Item_Type;
         State : Heaps.State_Type;
      begin
         Heaps.Get(Heap, Addresses(I), Item, State);
         if State /= Heaps.Success or else Item /= Make_Item(I) then
            Put_Line("Get"& I'Img & To_String(Item) &" = "& State'Img);
         end if;
      end;
   end loop;

   --Put_Line("LULU5");
   --Heaps.Print_Info_Index(Heap);
   --Heaps.Print_Free_Index(Heap);

   Heaps.Finalize(Heap);

exception
   when Error : others =>
      Put_Line("Exception: "& Exception_Information(Error));
      raise;
end Heap;

