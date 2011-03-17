-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.DSA.Gen_Heaps is

   procedure Create
     (Heap : in out Heap_Type;
      ID   : in     String)
   is
      pragma Precondition (not Heap.Initialized);
      pragma Precondition (not Heap.Finalized);
   begin
      Heap.Initialized := False;
      Block_IO.Create (ID, Heap.File);
      Heap.Initialized := True;
   end Create;


   procedure Create_Temporary
     (Heap : in out Heap_Type;
      ID   : in     String)
   is
      pragma Precondition (not Heap.Initialized);
      pragma Precondition (not Heap.Finalized);
   begin
      Heap.Initialized := False;
      Block_IO.Create_And_Open_Temporary (ID, Heap.File);
      Heap.Initialized := True;
   end Create_Temporary;


   procedure Open
     (Heap : in out Heap_Type;
      ID   : in     String)
   is
      pragma Precondition (not Heap.Initialized);
      pragma Precondition (not Heap.Finalized);
   begin
      Heap.Initialized := False;
      Block_IO.Open (ID, Heap.File);
      Heap.Initialized := True;
   end Open;


   procedure Finalize
     (Heap : in out Heap_Type) is
   begin
      if Heap.Initialized then
         Block_IO.Close (Heap.File);
         Heap.Initialized := False;
      end if;
      Heap.Finalized := True;
   end Finalize;


   procedure Read
     (Heap  : in out Heap_Type;
      Start : in     Valid_Address_Type;
      Item  :    out Item_Type;
      State :    out State_Type)
   is
      Block    : Blocks.Block_Type;
      Address  : Block_IO.Valid_Address_Type := Start.Address;
      Position : Blocks.Base_Position_Type := Start.Position;
      Context  : Read_Context_Type := New_Read_Context;
   begin
      Read_Item : loop
         Block_IO.Read (Heap.File, Address, Block);
         declare
            Cursor : Blocks.Cursor_Type := Blocks.New_Cursor (Position);
            Done   : Boolean;
         begin
            Eat_Block : loop
               Read_Part_Of_Item (Context, Block, Cursor, Item, Done);
               exit Eat_Block when not Blocks.Is_Valid (Cursor);
               exit Read_Item when Done;
            end loop Eat_Block;
         end;
         Address  := Block_IO.Succ (Address);
         Position := Block'First;
      end loop Read_Item;
      State := Success;
   end Read;


   procedure Append
     (Heap    : in out Heap_Type;
      Item    : in     Item_Type;
      Start   : in     Valid_Address_Type;
      State   :    out State_Type)
   is
      Block    : Blocks.Block_Type;
      Address  : Block_IO.Valid_Address_Type;
      Position : Blocks.Base_Position_Type;
      Context  : Write_Context_Type := New_Write_Context;
   begin
      null;
      -- Create some of the blocks and flush them to disk iteratively.
      -- But then we need a next-pointer in each block, hence we need even more
      -- Seek + Write operations (to set the next-pointer).
      -- This could work well if reading blocks in descending order would not
      -- require seeks. Check this.

      -- something like this to get to the end:
      --<<Retry>>
      --Block_IO.Seek_Last (Heap.File, Address);
      --Block_IO.Lock (Heap.File, Address);
      --declare
         --Test_Address : Block_IO.Valid_Address_Type;
      --begin
         --Block_IO.Seek_Last (Heap.File, Test_Address);
         --if Address /= Test_Address then
            --goto Retry;
         --end if;
      --end;
   end Append;


   procedure Delete
     (Heap  : in out Heap_Type;
      Start : in     Valid_Address_Type;
      State :    out State_Type) is
   begin
      null;
   end Delete;

end DB.DSA.Gen_Heaps;

