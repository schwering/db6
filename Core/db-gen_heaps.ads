-- Abstract:
--
-- A heap is capable of storing large data values.
--
-- The insertion process returns an address. This address can be used to read
-- the value later.
--
-- Design Notes:
--
-- (TODO document)
--
-- Copyright 2008, 2009 Christoph Schwering

with System.Storage_Elements;
with System.Storage_Pools;

with DB.Gen_BTrees;
with DB.IO.Blocks;
with DB.IO.Blocks.Gen_Buffers;
with DB.IO.Blocks.Gen_IO;

generic
   type Item_Type (<>) is private; -- maybe, (<>) should be removed
   with function To_Storage_Array
          (Item : Item_Type)
           return System.Storage_Elements.Storage_Array;
   with function From_Storage_Array
          (Arr : System.Storage_Elements.Storage_Array)
           return Item_Type;
   with function Info_Index_ID
          (ID : String)
           return String;
   with function Free_Index_ID
          (ID : String)
           return String;

   Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;

   with package Block_IO is new IO.Blocks.Gen_IO (<>);
package DB.Gen_Heaps is
   --pragma Preelaborate;

   subtype Address_Type is Block_IO.Valid_Address_Type;
   type Heap_Type is limited private;
   type Transaction_Type is abstract tagged limited private;
   type RO_Transaction_Type is new Transaction_Type with private;
   type RW_Transaction_Type is new Transaction_Type with private;
   type Result_Type is (Success, Failure, Error);

   Heap_Error : exception;
   -- This exception is only raised when there are extremely serious
   -- errors in the heap such as dangling references to child or neighbor
   -- nodes.


   -- Heap initialization procedures: Create, Initialize, Finalize.

   procedure Create
     (ID : in String);
   -- Creates a new tree named ID or raises a DB.IO.IO_Error when creation
   -- fails.

   procedure Initialize
     (Heap : out Heap_Type;
      ID   : in  String);
   -- Initializes Heap with the tree named ID.

   procedure Finalize
     (Heap : in out Heap_Type);
   -- Finalizes Heap, i.e. closes opened files.


   -- Transaction: New_R[O|W]Transaction, Start_Transaction, Abort_Transaction,
   -- Commit_Transaction.

   function New_RO_Transaction
     (Heap : Heap_Type)
      return RO_Transaction_Type;

   procedure Start_Transaction
     (Heap        : in out Heap_Type;
      Transaction : in out RO_Transaction_Type);

   procedure Finish_Transaction
     (Heap        : in out Heap_Type;
      Transaction : in out RO_Transaction_Type);

   function New_RW_Transaction
     (Heap : Heap_Type)
      return RW_Transaction_Type;

   procedure Start_Transaction
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type);

   procedure Abort_Transaction
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type);

   procedure Commit_Transaction
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type);


   -- Operations: Get, Put and Delete.

   procedure Get
     (Heap    : in out Heap_Type;
      Address : in     Address_Type;
      Item    :    out Item_Type;
      State   :    out Result_Type);
   -- Get the Item stored under Address or set State = Failure if no such
   -- Item exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Get
     (Heap        : in out Heap_Type;
      Transaction : in out Transaction_Type'Class;
      Address     : in     Address_Type;
      Item        :    out Item_Type;
      State       :    out Result_Type);
   -- Get the Item stored under Address or set State = Failure if no such
   -- Item exists.

   procedure Put
     (Heap    : in out Heap_Type;
      Item    : in     Item_Type;
      Address :    out Address_Type;
      State   :    out Result_Type);
   -- Inserts Item and sets Address to the address of Item.
   -- Generally, State = Failure should never occur because there is nothing
   -- that could fail.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Put
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Item        : in     Item_Type;
      Address     :    out Address_Type;
      State       :    out Result_Type);
   -- Inserts Item and sets Address to the address of Item.
   -- Generally, State = Failure should never occur because there is nothing
   -- that could fail.

   procedure Delete
     (Heap    : in out Heap_Type;
      Address : in     Address_Type;
      State   :    out Result_Type);
   -- Deletes Item at Address and set Item to the deleted one or set
   -- State = Failure if no item exists on Address.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Delete
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Address     : in     Address_Type;
      State       :    out Result_Type);
   -- Deletes Item at Address and set Item to the deleted one or set
   -- State = Failure if no item exists on Address.

   procedure Print_Info_Index
     (Heap : in out Heap_Type);
   procedure Print_Free_Index
     (Heap : in out Heap_Type);

private
   type Offset_Type is mod IO.Blocks.Block_Size - 1;
   type Length_Type is mod 2**64;
   type Chunk_State_Type is (Free, Used);
   type Chunk_Info_Type is
      record
         State  : Chunk_State_Type;
         Length : Length_Type;
      end record;
   pragma Pack (Chunk_Info_Type);


   package Info_BTree_Types is
      subtype Key_Type is Address_Type;
      type Key_Context_Type is null record;

      subtype Value_Type is Chunk_Info_Type;
      type Value_Context_Type is null record;

      function "=" (A, B : Key_Type) return Boolean;
      function "<=" (A, B : Key_Type) return Boolean;
      pragma Inline ("<=");

      procedure Read_Key
        (Context : in out Key_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Key     :    out Key_Type);

      procedure Skip_Key
        (Context : in out Key_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type);

      procedure Write_Key
        (Context : in out Key_Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Key     : in     Key_Type);

      procedure Read_Value
        (Context : in out Value_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Value   :    out Value_Type);

      procedure Skip_Value
        (Context : in out Value_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type);

      procedure Write_Value
        (Context : in out Value_Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Value   : in     Value_Type);
   end Info_BTree_Types;


   package Info_BTrees is new Gen_BTrees
     (Key_Type                      => Info_BTree_Types.Key_Type,
      Key_Context_Type              => Info_BTree_Types.Key_Context_Type,
      Write_Key                     => Info_BTree_Types.Write_Key,
      Read_Key                      => Info_BTree_Types.Read_Key,
      Skip_Key                      => Info_BTree_Types.Skip_Key,
      "="                           => Info_BTree_Types."=",
      "<="                          => Info_BTree_Types."<=",
      Value_Type                    => Info_BTree_Types.Value_Type,
      Value_Context_Type            => Info_BTree_Types.Value_Context_Type,
      Write_Value                   => Info_BTree_Types.Write_Value,
      Read_Value                    => Info_BTree_Types.Read_Value,
      Skip_Value                    => Info_BTree_Types.Skip_Value,
      Is_Context_Free_Serialization => True,
      Storage_Pool                  => Storage_Pool,
      Block_IO                      => Block_IO);

   package Free_BTree_Types is
      type Key_Type is
         record
            Length  : Length_Type;
            Address : Address_Type;
         end record;
      pragma Pack (Key_Type);
      type Key_Context_Type is null record;

      type Value_Type is null record;
      type Value_Context_Type is null record;

      function "=" (A, B : Key_Type) return Boolean;
      pragma Inline ("=");
      function "<=" (A, B : Key_Type) return Boolean;
      pragma Inline ("<=");

      procedure Read_Key
        (Context : in out Key_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Key     :    out Key_Type);

      procedure Skip_Key
        (Context : in out Key_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type);

      procedure Write_Key
        (Context : in out Key_Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Key     : in     Key_Type);

      procedure Read_Value
        (Context : in out Value_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Value   :    out Value_Type);

      procedure Skip_Value
        (Context : in out Value_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type);

      procedure Write_Value
        (Context : in out Value_Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Value   : in     Value_Type);
   end Free_BTree_Types;

   package Free_BTrees is new Gen_BTrees
     (Key_Type                      => Free_BTree_Types.Key_Type,
      Key_Context_Type              => Free_BTree_Types.Key_Context_Type,
      Write_Key                     => Free_BTree_Types.Write_Key,
      Read_Key                      => Free_BTree_Types.Read_Key,
      Skip_Key                      => Free_BTree_Types.Skip_Key,
      "="                           => Free_BTree_Types."=",
      "<="                          => Free_BTree_Types."<=",
      Value_Type                    => Free_BTree_Types.Value_Type,
      Value_Context_Type            => Free_BTree_Types.Value_Context_Type,
      Write_Value                   => Free_BTree_Types.Write_Value,
      Read_Value                    => Free_BTree_Types.Read_Value,
      Skip_Value                    => Free_BTree_Types.Skip_Value,
      Is_Context_Free_Serialization => True,
      Storage_Pool                  => Storage_Pool,
      Block_IO                      => Block_IO);


   type Heap_Ref_Type is access all Heap_Type;
   pragma Controlled (Heap_Ref_Type);
   for Heap_Ref_Type'Storage_Size use 0;

   type Heap_Type is limited
      record
         File        : Block_IO.File_Type;
         Info_Tree   : Info_BTrees.Tree_Type;
         Free_Tree   : Free_BTrees.Tree_Type;
         Self        : Heap_Ref_Type := Heap_Type'Unchecked_Access;
         Initialized : Boolean := False;
         Finalized   : Boolean := False;
         -- Self is only for cleanup of Transaction_Type (i.e.
         -- their Finalize procedures), for absolutely nothing else.
      end record;


   function Block_Identity
     (Block : IO.Blocks.Block_Type)
      return IO.Blocks.Block_Type;
   pragma Inline (Block_Identity);

   -- All output operations use this buffer instead of direct IO so that they
   -- can commit *all* changes at the end to avoid an inconsistent heap.
   package IO_Buffers is new IO.Blocks.Gen_Buffers
     (Block_IO          => Block_IO,
      Item_Type         => IO.Blocks.Block_Type,
      To_Block          => Block_Identity,
      From_Block        => Block_Identity,
      Item_Storage_Pool => Storage_Pool,
      Node_Storage_Pool => Storage_Pool);
   subtype Block_Constant_Ref is IO_Buffers.Item_Constant_Ref_Type;

   type Transaction_Type is abstract tagged limited
      record
         Ticket           : Block_IO.Ticket_Type;
         Owning_Heap      : Heap_Ref_Type;
         Initialized      : Boolean := False;
         Started          : Boolean := False;
      end record;

   type RO_Transaction_Type is new Transaction_Type with
      record
         Info_Transaction : aliased Info_BTrees.RO_Transaction_Type;
      end record;

   type RW_Transaction_Type is new Transaction_Type with
      record
         Buffer           : IO_Buffers.Buffer_Type;
         Info_Transaction : Info_BTrees.RW_Transaction_Type;
         Free_Transaction : Free_BTrees.RW_Transaction_Type;
      end record;

end DB.Gen_Heaps;

