-- Abstract:
--
-- A heap is capable of storing large data values.
--
-- The insertion process returns an address. This address can be used to read
-- the value later.
--
-- Design Notes:
--
-- There are three different terms related with storage in this heap:
-- blocks, chunks and items.
-- A block is a simple disk block of fixed size. A chunk is a sequence of
-- N blocks where the address of the (I+1)-th block is exactly Succ of the
-- address of the I-th block. An item can be stored on one or more chunks.
--
-- The general idea is to have each item be stored in a single chunk. When an
-- item is deleted, the chunk is marked as free and merged with its preceding
-- and succeeding free chunks if they exist. The waste for each stored item is
-- at most (Block_Size - 1).
-- This idea was softened for the Append operation. In some cases, the appendum
-- may fit into the waste space of the chunk. If it does not, a new chunk
-- is allocated for the appendum (or, more exaclty, for that part of the
-- appendum that does not fit into the waste space) and linked with that chunk
-- of the item that has been the last one up to now. Hence, the chunks form a
-- simple linked list.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Storage_Pools;

with DB.Gen_BTrees;
with DB.IO.Blocks;
with DB.IO.Blocks.Gen_Buffers;
with DB.IO.Blocks.Gen_IO;

generic
   type Item_Type is private;
   type Item_Context_Type is private;
   with function Item_Size_Bound
          (Item : Item_Type)
           return IO.Blocks.Size_Type;
   with function Fold_Contexts
          (Left     : Item_Context_Type;
           Appended : Item_Context_Type)
           return Item_Context_Type;
   with function Context_Size_Bound
          (Context : Item_Context_Type)
           return IO.Blocks.Size_Type;
   with procedure Read_Context
          (Block   : in     IO.Blocks.Base_Block_Type;
           Cursor  : in out IO.Blocks.Cursor_Type;
           Context :    out Item_Context_Type);
   with procedure Write_Context
          (Block   : in out IO.Blocks.Base_Block_Type;
           Cursor  : in out IO.Blocks.Cursor_Type;
           Context : in     Item_Context_Type);
   with procedure Read_Part_Of_Item
          (Context : in out Item_Context_Type;
           Block   : in     IO.Blocks.Base_Block_Type;
           Cursor  : in out IO.Blocks.Cursor_Type;
           Item    : in out Item_Type;
           Done    :    out Boolean);
   with procedure Write_Part_Of_Item
          (Context : in out Item_Context_Type;
           Block   : in out IO.Blocks.Base_Block_Type;
           Cursor  : in out IO.Blocks.Cursor_Type;
           Item    : in     Item_Type;
           Done    :    out Boolean);

   with function Info_Index_ID
          (ID : String)
           return String;
   with function Free_Index_ID
          (ID : String)
           return String;

   Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;

   with package Block_IO is new IO.Blocks.Gen_IO (<>);
package DB.Gen_Heaps is
   pragma Elaborate_Body;

   ----------
   -- Heap initialization operations.

   type Heap_Type is limited private;

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

   ----------
   -- Transactions and their operations.

   type Transaction_Type is abstract tagged limited private;
   type RO_Transaction_Type is new Transaction_Type with private;
   type RW_Transaction_Type is new Transaction_Type with private;

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

   ----------
   -- Core operations: Get, Put and Delete.

   subtype Address_Type is Block_IO.Valid_Address_Type;
   type State_Type is (Success, Failure, Error);

   procedure Get
     (Heap    : in out Heap_Type;
      Address : in     Address_Type;
      Item    :    out Item_Type;
      State   :    out State_Type);
   -- Get the Item stored under Address or set State = Failure if no such
   -- Item exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Get
     (Heap        : in out Heap_Type;
      Transaction : in out Transaction_Type'Class;
      Address     : in     Address_Type;
      Item        :    out Item_Type;
      State       :    out State_Type);
   -- Get the Item stored under Address or set State = Failure if no such
   -- Item exists.

   procedure Put
     (Heap    : in out Heap_Type;
      Item    : in     Item_Type;
      Address :    out Address_Type;
      State   :    out State_Type);
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
      State       :    out State_Type);
   -- Inserts Item and sets Address to the address of Item.
   -- Generally, State = Failure should never occur because there is nothing
   -- that could fail.

   procedure Append
     (Heap        : in out Heap_Type;
      Item        : in     Item_Type;
      Address     : in     Address_Type;
      State       :    out State_Type);
   -- Appends Item and to the item stored at Address or sets State = Failure
   -- if no item exists on Address.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Append
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Item        : in     Item_Type;
      Address     : in     Address_Type;
      State       :    out State_Type);
   -- Appends Item and to the item stored at Address or sets State = Failure
   -- if no item exists on Address.

   procedure Delete
     (Heap    : in out Heap_Type;
      Address : in     Address_Type;
      State   :    out State_Type);
   -- Deletes Item at Address and set Item to the deleted one or set
   -- State = Failure if no item exists on Address.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Delete
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Address     : in     Address_Type;
      State       :    out State_Type);
   -- Deletes Item at Address and set Item to the deleted one or set
   -- State = Failure if no item exists on Address.

   ----------
   -- Some debug procedures.

   procedure Print_Info_Index
     (Heap : in out Heap_Type);

   procedure Print_Free_Index
     (Heap : in out Heap_Type);

private
   Kilo : constant := 2**10;
   Mega : constant := 2**10 * Kilo;
   Giga : constant := 2**10 * Mega;
   Tera : constant := 2**10 * Giga;

   type Length_Type is mod 2**62;
   -- Remember that Length_Type is the length of possibly really long chunks
   -- which can be the complete file (when it's empty).

   subtype Extended_Address_Type is Block_IO.Address_Type;

   type Chunk_State_Type is (Free, Used_First, Used_Cont);
   type Chunk_Info_Type is
      record
         State   : Chunk_State_Type;
         Length  : Length_Type;
         Succ    : Extended_Address_Type;
         Last    : Address_Type;
         Context : Item_Context_Type;
      end record;


   package Info_BTree_Types is
      subtype Key_Type is Address_Type;
      type Key_Context_Type is null record;

      subtype Value_Type is Chunk_Info_Type;
      type Value_Context_Type is null record;

      function "=" (A, B : Key_Type) return Boolean;
      function "<=" (A, B : Key_Type) return Boolean;
      pragma Inline ("<=");

      function Key_Size_Bound
        (Key : Key_Type)
         return IO.Blocks.Size_Type;

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

      function Value_Size_Bound
        (Value : Value_Type)
         return IO.Blocks.Size_Type;

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
      Key_Size_Bound                => Info_BTree_Types.Key_Size_Bound,
      Write_Key                     => Info_BTree_Types.Write_Key,
      Read_Key                      => Info_BTree_Types.Read_Key,
      Skip_Key                      => Info_BTree_Types.Skip_Key,
      "="                           => Info_BTree_Types."=",
      "<="                          => Info_BTree_Types."<=",
      Value_Type                    => Info_BTree_Types.Value_Type,
      Value_Context_Type            => Info_BTree_Types.Value_Context_Type,
      Value_Size_Bound              => Info_BTree_Types.Value_Size_Bound,
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

      function Key_Size_Bound
        (Key : Key_Type)
         return IO.Blocks.Size_Type;

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

      function Value_Size_Bound
        (Value : Value_Type)
         return IO.Blocks.Size_Type;

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
      Key_Size_Bound                => Free_BTree_Types.Key_Size_Bound,
      Write_Key                     => Free_BTree_Types.Write_Key,
      Read_Key                      => Free_BTree_Types.Read_Key,
      Skip_Key                      => Free_BTree_Types.Skip_Key,
      "="                           => Free_BTree_Types."=",
      "<="                          => Free_BTree_Types."<=",
      Value_Type                    => Free_BTree_Types.Value_Type,
      Value_Context_Type            => Free_BTree_Types.Value_Context_Type,
      Value_Size_Bound              => Free_BTree_Types.Value_Size_Bound,
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

