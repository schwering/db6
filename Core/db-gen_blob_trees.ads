-- Abstract:
--
-- A map data structure similar to Gen_BTrees that stores values of unbounded
-- size.
--
-- Design Notes:
--
-- This package makes use of Gen_BTrees and Gen_Heaps: Each Key/Value pair is
-- mapped to a Key/Ref entry in a BTree and Value at position Ref in the Heap.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Gen_BTrees;
with DB.Gen_Heaps;
with DB.IO.Blocks;
with DB.IO.Blocks.Gen_IO;
with DB.IO.Blocks.Gen_IO.Gen_Buffers;
with DB.Utils;

generic
   type Key_Type is private;
   type Value_Type is private;

   with function Compare
     (Left, Right : Key_Type)
      return Utils.Comparison_Result_Type;

   Allow_Duplicates : in Boolean := False;

   type Key_Context_Type is private;
   with function New_Key_Context
      return Key_Context_Type;
   with function Key_Size_Bound
     (Key : Key_Type)
      return IO.Blocks.Size_Type;
   with procedure Read_Key
     (Context : in out Key_Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Key     :    out Key_Type);
   with procedure Skip_Key
     (Context : in out Key_Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type);
   with procedure Write_Key
     (Context : in out Key_Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Key     : in     Key_Type);


   type Value_Context_Type is private;
   with function New_Value_Context
      return Value_Context_Type;
   with function Value_Size_Bound
     (Value : Value_Type)
      return IO.Blocks.Size_Type;
   with procedure Read_Value
     (Context : in out Value_Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Value   :    out Value_Type);
   with procedure Skip_Value
     (Context : in out Value_Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type);
   with procedure Write_Value
     (Context : in out Value_Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Value   : in     Value_Type);

   type Parted_Value_Context_Type is private;
   with function New_Parted_Value_Context
     return Parted_Value_Context_Type;
   with function Parted_Value_Size_Bound
     (Value : Value_Type)
      return IO.Blocks.Size_Type;
   with function Fold_Value_Contexts
     (Left     : Parted_Value_Context_Type;
      Appended : Parted_Value_Context_Type)
      return Parted_Value_Context_Type;
   with function Value_Context_Size_Bound
     (Parted_Value_Context : Parted_Value_Context_Type)
      return IO.Blocks.Size_Type;
   with procedure Read_Value_Context
     (Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Context :    out Parted_Value_Context_Type);
   with procedure Write_Value_Context
     (Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Context : in     Parted_Value_Context_Type);
   with procedure Read_Part_Of_Value
     (Context : in out Parted_Value_Context_Type;
      Block   : in     IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Value   : in out Value_Type;
      Done    :    out Boolean);
   with procedure Write_Part_Of_Value
     (Context : in out Parted_Value_Context_Type;
      Block   : in out IO.Blocks.Base_Block_Type;
      Cursor  : in out IO.Blocks.Cursor_Type;
      Value   : in     Value_Type;
      Done    :    out Boolean);

   Is_Context_Free_Serialization : in Boolean;

   with package Block_IO is new IO.Blocks.Gen_IO (<>);
   with package IO_Buffers is new Block_IO.Gen_Buffers (<>);
package DB.Gen_Blob_Trees is
   pragma Elaborate_Body;

   ----------
   -- Tree initialization procedures.

   type Tree_Type is limited private;

   procedure Create
     (ID : in String);
   -- Creates a new tree named ID or raises a DB.IO.IO_Error when creation
   -- fails.

   procedure Initialize
     (Tree : out Tree_Type;
      ID   : in  String);
   -- Initializes Tree with the tree named ID.

   procedure Finalize
     (Tree : in out Tree_Type);
   -- Finalizes Tree, i.e. closes opened files.

   function Max_Key_Size
      return IO.Blocks.Size_Type;
   -- Returns the maximum allowed size of keys.

   ----------
   -- Transactions and their operations.

   type Transaction_Type is abstract tagged limited private;
   type RO_Transaction_Type is new Transaction_Type with private;
   type RW_Transaction_Type is new Transaction_Type with private;

   function New_RO_Transaction
     (Tree : Tree_Type)
      return RO_Transaction_Type;

   procedure Start_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type);

   procedure Finish_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type);

   function New_RW_Transaction
     (Tree : Tree_Type)
      return RW_Transaction_Type;

   procedure Start_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type);

   procedure Abort_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type);

   procedure Commit_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type);

   ----------
   -- Core operations: Retrieve, Insertion, Deletion.

   type State_Type is (Success, Failure, Error);

   procedure Retrieve
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type);
   -- Searches the Value associated with Key or sets State = Failure if
   -- no such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Retrieve
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);
   -- Searches the Value associated with Key or sets State = Failure if
   -- no such key exists.

   procedure Minimum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type);
   -- Searches the minimum Key / Value pair or sets State = Failure if no
   -- such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Minimum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);
   -- Searches the minimum Key / Value pair or sets State = Failure if no
   -- such key exists.

   procedure Maximum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type);
   -- Searches the maximum Key / Value pair or sets State = Failure if no
   -- such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Maximum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);
   -- Searches the maximum Key / Value pair or sets State = Failure if no
   -- such key exists.

   procedure Insert
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      State    :    out State_Type);
   -- Inserts a Key / Value pair or sets State = Failure if such a key already
   -- exists.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Insert
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      State       :    out State_Type);
   -- Inserts a Key / Value pair or sets State = Failure if such a key already
   -- exists.

   procedure Append
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      State    :    out State_Type);
   -- Appends the Value as part to the value stored under Key as long as this
   -- value is stored indirectly, i.e. in the heap. In all other cases,
   -- including when the value is stored directly in the BTree leaves,
   -- State = Failure, or in case of serious errors State = Error.

   procedure Append
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      State       :    out State_Type);
   -- Appends the Value as part to the value stored under Key as long as this
   -- value is stored indirectly, i.e. in the heap. In all other cases,
   -- including when the value is stored directly in the BTree leaves,
   -- State = Failure, or in case of serious errors State = Error.

   procedure Delete
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      State    :    out State_Type);
   -- Deletes the Key / Value pair or sets State = Failure if no such key
   -- exists.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);
   -- Deletes the Key / Value pair or sets State = Failure if no such key
   -- exists.

   ----------
   -- Miscellaneous information procedures.

   subtype Count_Type is Natural;

   procedure Count
     (Tree  : in out Tree_Type;
      Count :    out Count_Type);
   -- Determines the count N of key / value pairs in the tree.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Count
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Count       :    out Count_Type);
   -- Determines the count N of key / value pairs in the tree.

   procedure Get_Height
     (Tree   : in out Tree_Type;
      Height :    out Natural);
   -- Determines the Height of the tree.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Get_Height
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Natural);
   -- Determines the Height of the tree.

   procedure Clusterize
     (Tree  : in out Tree_Type;
      State :    out State_Type);
   -- Reorganizes the nodes in the file.
   -- Not implemented yet.

   ----------
   -- Cursor operations.

   type Comparison_Type is (Less, Less_Or_Equal, Equal, Greater_Or_Equal,
      Greater);
   type Bound_Type is private;
   type Cursor_Type is limited private;

   function Positive_Infinity_Bound
      return Bound_Type;
   -- Returns an abstract bound that means positive infinity.

   function Negative_Infinity_Bound
      return Bound_Type;
   -- Returns an abstract bound that means negative infinity.

   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type;
   -- Creates a concrete bound. The bound New_Bound(Greater, Min) is a
   -- lower bound, because this means that all keys Key hit by the cursor
   -- must satisfy: Key > Min.

   function New_Cursor
     (Tree              : Tree_Type;
      Transaction       : Transaction_Type'Class;
      Thread_Safe       : Boolean;
      Lower_Bound       : Bound_Type;
      Upper_Bound       : Bound_Type;
      Reverse_Direction : Boolean := False)
      return Cursor_Type;
   -- Creates a new cursor. The returned cursor relies on a Transaction. This
   -- Transaction must be initialized, of course.
   -- If Thread_Safe is True, all operations on the cursor happen mutually
   -- exclusive.
   -- All key/value-pairs hit by the cursor satisfy Lower_Bound and Upper_Bound.
   -- If Reverse_Direction is True, the cursor moves from the maximum to the
   -- minimum instead of the other way around.
   -- Note that generally cursors with Transactions are not such a great
   -- idea. Remind the possibly large transaction if, for example, all
   -- elements visited by the cursor are Deleted.
   -- The rule of thumb is generally: a transaction should be never created
   -- for a cursor. However, a transactional cursor might be necessary if the
   -- cursor must consider changes of the data that happened in the transaction
   -- but were not yet committed.

   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean);
   -- If thread safety is enabled, all operations of the cursor are mutually
   -- exclusive.

   procedure Finalize_Cursor
     (Tree        : in     Tree_Type;
      Transaction : in     Transaction_Type'Class;
      Cursor      : in out Cursor_Type);
   -- Releases all resources hold be the cursor. In particular, this is a
   -- small amount of memory (the cursors use the heap; it would be possible
   -- without the heap, but probably not faster) and, in case of the
   -- non-transactional cursors, the unlock of the locks and release of the
   -- ticket.

   procedure Pause
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type);
   -- Releases the lock hold by Cursor. The next Next or Unpause call will
   -- acquire the needed read-lock again. Furthermore, the next Next call will
   -- try to recalibrate the Cursor, i.e. find the key/value-pair that should
   -- be visited next. Hence, it is allowed to delete elements from Tree
   -- in between two Pause and Unpause calls.

   procedure Unpause
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type);
   -- Acquires the needed read-lock again after a Pause call.

   procedure Next
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);
   -- Steps forward to the next Key/Value-pair and sets State to Success.
   -- If no such pair exists (with regard to the set bounds), State is set to
   -- Failure or Error.
   -- If Delete was called before, Next must recalibrate, i.e. find the
   -- key/value-pair that should be visited next.

   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out State_Type);
   -- Deletes the Key/Value-pair which was last hit by Cursor and sets State
   -- to the outcome of the deletion. If there was no previous Next call or it
   -- was not successful, the deletion is not successful, either, and State is
   -- set to Failure.
   -- The next Next operation will need to recalibrate, i.e. find the
   -- key/value-pair that should be visited next.

private
   package Heap_Utils is
      function Info_Index_ID
        (ID : String)
         return String;

      function Free_Index_ID
        (ID : String)
         return String;
   end Heap_Utils;

   package Heaps is new Gen_Heaps
     (Item_Type          => Value_Type,
      New_Item_Context   => New_Parted_Value_Context,
      Item_Context_Type  => Parted_Value_Context_Type,
      Item_Size_Bound    => Parted_Value_Size_Bound,
      Fold_Contexts      => Fold_Value_Contexts,
      Context_Size_Bound => Value_Context_Size_Bound,
      Read_Context       => Read_Value_Context,
      Write_Context      => Write_Value_Context,
      Read_Part_Of_Item  => Read_Part_Of_Value,
      Write_Part_Of_Item => Write_Part_Of_Value,
      Info_Index_ID      => Heap_Utils.Info_Index_ID,
      Free_Index_ID      => Heap_Utils.Free_Index_ID,
      Block_IO           => Block_IO,
      IO_Buffers         => IO_Buffers);

   package BTree_Utils is
      type Value_Type (Direct : Boolean := True) is
         record
            case Direct is
               when True  => Value   : Gen_Blob_Trees.Value_Type;
               when False => Address : Heaps.Address_Type;
            end case;
         end record;

      type Context_Type is
         record
            Value_Context : Value_Context_Type;
         end record;

      function Max_Key_Size
         return IO.Blocks.Size_Type;

      function Fits_Direct
        (Key   : Gen_Blob_Trees.Key_Type;
         Value : Gen_Blob_Trees.Value_Type)
         return Boolean;

      function New_Value_Context
         return Context_Type;

      function Value_Size_Bound
        (Value : Value_Type)
         return IO.Blocks.Size_Type;

      procedure Read_Value
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Value   :    out Value_Type);

      procedure Skip_Value
        (Context : in out Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type);

      procedure Write_Value
        (Context : in out Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Value   : in     Value_Type);
   end BTree_Utils;

   package BTrees is new Gen_BTrees
     (Key_Type                      => Key_Type,
      Value_Type                    => BTree_Utils.Value_Type,
      Compare                       => Compare,
      Allow_Duplicates              => Allow_Duplicates,
      Key_Context_Type              => Key_Context_Type,
      New_Key_Context               => New_Key_Context,
      Key_Size_Bound                => Key_Size_Bound,
      Read_Key                      => Read_Key,
      Skip_Key                      => Skip_Key,
      Write_Key                     => Write_Key,
      Value_Context_Type            => BTree_Utils.Context_Type,
      New_Value_Context             => BTree_Utils.New_Value_Context,
      Value_Size_Bound              => BTree_Utils.Value_Size_Bound,
      Read_Value                    => BTree_Utils.Read_Value,
      Skip_Value                    => BTree_Utils.Skip_Value,
      Write_Value                   => BTree_Utils.Write_Value,
      Is_Context_Free_Serialization => Is_Context_Free_Serialization,
      Block_IO                      => Block_IO,
      IO_Buffers                    => IO_Buffers);

   type Tree_Type is limited
      record
         BTree : BTrees.Tree_Type;
         Heap  : Heaps.Heap_Type;
      end record;

   type Transaction_Type is abstract tagged limited null record;

   type RO_Transaction_Type is new Transaction_Type with
      record
         BTree_Transaction : BTrees.RO_Transaction_Type;
         Heap_Transaction  : Heaps.RO_Transaction_Type;
      end record;

   type RW_Transaction_Type is new Transaction_Type with
      record
         BTree_Transaction : BTrees.RW_Transaction_Type;
         Heap_Transaction  : Heaps.RW_Transaction_Type;
      end record;

   type Bound_Type is new BTrees.Bound_Type;

   type Cursor_Type is limited
      record
         Cursor : BTrees.Cursor_Type;
      end record;

end DB.Gen_Blob_Trees;

