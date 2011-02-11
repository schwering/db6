-- Abstract:
--
-- A heap is capable of storing large data values.
--
-- The implementation is rather simple with respect to reclaiming unused space.
-- The idea is to reorganize the heap once in a while.
--
-- With the old, transaction-oriented B-tree there was an also
-- transaction-oriented heap implementation which had more sophisticated
-- features like memory reclaiming.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Blocks;
with DB.Blocks.Gen_IO_Signature;

generic
   type Item_Type is private;
   type Read_Context_Type (<>) is private;
   type Write_Context_Type (<>) is private;

   with function New_Read_Context return Read_Context_Type is <>;

   with function New_Write_Context return Write_Context_Type is <>;

   with procedure Read_Part_Of_Item
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Item    : in out Item_Type;
      Done    :    out Boolean) is <>;

   with procedure Write_Part_Of_Item
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Item    : in     Item_Type;
      Done    :    out Boolean) is <>;

   with package Block_IO is new Blocks.Gen_IO_Signature (<>);
package DB.DSA.Gen_Heaps is
   pragma Preelaborate;

   type Heap_Type is limited private;

   procedure Create
     (Heap : in out Heap_Type;
      ID   : in     String);
   -- Creates a new heap named ID or raises a DB.IO_Error when creation
   -- fails.

   procedure Create_Temporary
     (Heap : in out Heap_Type;
      ID   : in     String);
   -- Creates a new Heap named ID or raises a DB.IO_Error when creation
   -- fails.

   procedure Open
     (Heap : in out Heap_Type;
      ID   : in     String);
   -- Opens Heap with the heap named ID.

   procedure Finalize
     (Heap : in out Heap_Type);
   -- Finalizes Heap, i.e. closes opened files.

   type Valid_Address_Type is
      record
         Address  : Block_IO.Valid_Address_Type;
         Position : Blocks.Index_Type;
      end record;

   type State_Type is (Success, Error);

   procedure Read
     (Heap    : in out Heap_Type;
      Address : in     Valid_Address_Type;
      Item    :    out Item_Type;
      State   :    out State_Type);
   -- Reads the Item stored at Address or sets State = Failure if no such item
   -- exists. This procedure has undefined behaviour if Address doesn't actually
   -- point to the beginning of an item but somewhere into the data of an item.

   procedure Append
     (Heap    : in out Heap_Type;
      Item    : in     Item_Type;
      Address : in     Valid_Address_Type;
      State   :    out State_Type);
   -- Appends Item at Address or sets State = Failure

   procedure Delete
     (Heap    : in out Heap_Type;
      Address : in     Valid_Address_Type;
      State   :    out State_Type);
   -- Deletes Item at Address by overwriting its contents with zeros.
   -- State = Failure if no item exists on Address.

private
   type Heap_Type is
      record
         null;
      end record;

end DB.DSA.Gen_Heaps;

