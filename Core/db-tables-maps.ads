-- Abstract:
--
-- Wrapper for BTree and Blob_Tree. Decides which to use depending on the
-- maximum value size.
--
-- Design Notes:
--
-- Experimentally, I have removed the (<>) in the public type declarations and
-- given all Short discriminants the default value True. The idea is that this
-- allows to have objects of the types defined here as component in records.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Pool_Global;
with System.Storage_Pools;

use System.Pool_Global;
use System.Storage_Pools;

with DB.Gen_BTrees;
with DB.Gen_Blob_Trees;
with DB.IO.Blocks;
with DB.IO.Blocks.File_IO;
with DB.IO.Blocks.Gen_Simple_Buffers;

package DB.Tables.Maps is
   pragma Elaborate_Body;

   ----------
   -- Map initialization operations.

   type Map_Type is limited private;

   function New_Map
     (Max_Key_Size   : in IO.Blocks.Size_Type;
      Max_Value_Size : in IO.Blocks.Size_Type)
      return Map_Type;
   -- Initializes a map object.

   procedure Create
     (ID             : in String;
      Max_Key_Size   : in IO.Blocks.Size_Type;
      Max_Value_Size : in IO.Blocks.Size_Type);
   -- Creates a new map named ID or raises a DB.IO.IO_Error when creation
   -- fails.

   procedure Initialize
     (Map  : out Map_Type;
      ID   : in  String);
   -- Initializes Map with the map named ID.

   procedure Finalize
     (Map  : in out Map_Type);
   -- Finalizes Map, i.e. closes opened files.

   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : IO.Blocks.Size_Type)
      return IO.Blocks.Size_Type;
   -- Returns the maximum allowed size of keys.

   ----------
   -- Transactions and their operations.

   type Transaction_Type is abstract tagged limited private;
   type RO_Transaction_Type (<>) is new Transaction_Type with private;
   type RW_Transaction_Type (<>) is new Transaction_Type with private;

   function New_RO_Transaction
     (Map  : Map_Type)
      return RO_Transaction_Type;

   procedure Start_Transaction
     (Map         : in out Map_Type;
      Transaction : in out RO_Transaction_Type);

   procedure Finish_Transaction
     (Map         : in out Map_Type;
      Transaction : in out RO_Transaction_Type);

   function New_RW_Transaction
     (Map  : Map_Type)
      return RW_Transaction_Type;

   procedure Start_Transaction
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type);

   procedure Abort_Transaction
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type);

   procedure Commit_Transaction
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type);

   ----------
   -- Core operations: Retrieve, Insertion, Deletion.

   type State_Type is (Success, Failure, Error);

   procedure Retrieve
     (Map      : in out Map_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type);
   -- Searches the Value associated with Key or sets State = Failure if
   -- no such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Retrieve
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type'Class;
      State       :    out State_Type);
   -- Searches the Value associated with Key or sets State = Failure if
   -- no such key exists.

   procedure Minimum
     (Map      : in out Map_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type);
   -- Searches the minimum Key / Value pair or sets State = Failure if no
   -- such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Minimum
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type'Class;
      State       :    out State_Type);
   -- Searches the minimum Key / Value pair or sets State = Failure if no
   -- such key exists.

   procedure Maximum
     (Map      : in out Map_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type);
   -- Searches the maximum Key / Value pair or sets State = Failure if no
   -- such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Maximum
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type'Class;
      State       :    out State_Type);
   -- Searches the maximum Key / Value pair or sets State = Failure if no
   -- such key exists.

   procedure Insert
     (Map      : in out Map_Type;
      Key      : in     Key_Type;
      Value    : in     Value_Type'Class;
      State    :    out State_Type);
   -- Inserts a Key / Value pair or sets State = Failure if such a key already
   -- exists.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Insert
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type'Class;
      State       :    out State_Type);
   -- Inserts a Key / Value pair or sets State = Failure if such a key already
   -- exists.

   procedure Delete
     (Map      : in out Map_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type'Class;
      State    :    out State_Type);
   -- Deletes the Key / Value pair or sets State = Failure if no such key
   -- exists.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Delete
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type'Class;
      State       :    out State_Type);
   -- Deletes the Key / Value pair or sets State = Failure if no such key
   -- exists.

   ----------
   -- Miscellaneous information procedures.

   subtype Count_Type is Natural;

   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type);
   -- Determines the count N of key / value pairs in the map.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Count
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Count       :    out Count_Type);
   -- Determines the count N of key / value pairs in the map.

   procedure Get_Height
     (Map    : in out Map_Type;
      Height :    out Natural);
   -- Determines the Height of the map.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Get_Height
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Natural);
   -- Determines the Height of the map.

   procedure Clusterize
     (Map   : in out Map_Type;
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
     (Map : Map_Type)
      return Bound_Type;
   -- Returns an abstract bound that means positive infinity.

   function Negative_Infinity_Bound
     (Map : Map_Type)
      return Bound_Type;
   -- Returns an abstract bound that means negative infinity.

   function New_Bound
     (Map        : Map_Type;
      Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type;
   -- Creates a concrete bound. The bound New_Bound(Greater, Min) is a
   -- lower bound, because this means that all keys Key hit by the cursor
   -- must satisfy: Key > Min.

   function New_Cursor
     (Map               : Map_Type;
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
     (Map         : in     Map_Type;
      Transaction : in     Transaction_Type'Class;
      Cursor      : in out Cursor_Type);
   -- Releases all resources hold be the cursor. In particular, this is a
   -- small amount of memory (the cursors use the heap; it would be possible
   -- without the heap, but probably not faster) and, in case of the
   -- non-transactional cursors, the unlock of the locks and release of the
   -- ticket.

   procedure Pause
     (Map    : in out Map_Type;
      Cursor : in out Cursor_Type);
   -- Releases the lock hold by Cursor. The next Next or Unpause call will
   -- acquire the needed read-lock again. Furthermore, the next Next call will
   -- try to recalibrate the Cursor, i.e. find the key/value-pair that should
   -- be visited next. Hence, it is allowed to delete elements from Map
   -- in between two Pause and Unpause calls.

   procedure Unpause
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type);
   -- Acquires the needed read-lock again after a Pause call.

   procedure Next
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type'Class;
      State       :    out State_Type);
   -- Steps forward to the next Key/Value-pair and sets State to Success.
   -- If no such pair exists (with regard to the set bounds), State is set to
   -- Failure or Error.
   -- If Delete was called before, Next must recalibrate, i.e. find the
   -- key/value-pair that should be visited next.

   procedure Delete
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type'Class;
      State       :    out State_Type);
   -- Deletes the Key/Value-pair which was last hit by Cursor and sets State
   -- to the outcome of the deletion. If there was no previous Next call or it
   -- was not successful, the deletion is not successful, either, and State is
   -- set to Failure.
   -- The next Next operation will need to recalibrate, i.e. find the
   -- key/value-pair that should be visited next.

private
   package Bounded_Values_IO   renames Types.Values.Bounded.Uncompressed;
   package Unbounded_Values_IO renames Types.Values.Unbounded.Uncompressed;
   package Block_IO_Impl       renames IO.Blocks.File_IO;
   package Block_IO            renames Block_IO_Impl.IO;
   package IO_Buffers_Impl is new DB.IO.Blocks.Gen_Simple_Buffers
     (Block_IO          => Block_IO,
      Node_Storage_Pool => Root_Storage_Pool'Class(Global_Pool_Object));
   package IO_Buffers          renames IO_Buffers_Impl.Buffers;

   package BTrees is new Gen_BTrees
     (Key_Type           => Types.Keys.Key_Type,
      Value_Type         => Types.Values.Bounded.String_Type,
      Compare            => Types.Keys.Compare,
      Allow_Duplicates   => True,
      Key_Context_Type   => Types.Keys.Context_Type,
      New_Key_Context    => Types.Keys.New_Context,
      Key_Size_Bound     => Types.Keys.Size_Bound,
      Read_Key           => Types.Keys.Read,
      Skip_Key           => Types.Keys.Skip,
      Write_Key          => Types.Keys.Write,
      Value_Context_Type => Bounded_Values_IO.Context_Type,
      New_Value_Context  => Bounded_Values_IO.New_Context,
      Value_Size_Bound   => Bounded_Values_IO.Size_Bound,
      Read_Value         => Bounded_Values_IO.Read,
      Skip_Value         => Bounded_Values_IO.Skip,
      Write_Value        => Bounded_Values_IO.Write,
      Is_Context_Free_Serialization =>
              Types.Keys.Is_Context_Free_Serialization and
              Types.Values.Bounded.Uncompressed.Is_Context_Free_Serialization,
      Block_IO           => Block_IO,
      IO_Buffers         => IO_Buffers);

   package Blob_Trees is new Gen_Blob_Trees
     (Key_Type            => Types.Keys.Key_Type,
      Value_Type          => Types.Values.Unbounded.String_Type,
      Compare             => Types.Keys.Compare,
      Key_Context_Type    => Types.Keys.Context_Type,
      New_Key_Context     => Types.Keys.New_Context,
      Key_Size_Bound      => Types.Keys.Size_Bound,
      Read_Key            => Types.Keys.Read,
      Skip_Key            => Types.Keys.Skip,
      Write_Key           => Types.Keys.Write,
      Value_Context_Type  => Types.Values.Unbounded.Uncompressed.Context_Type,
      New_Value_Context   => Types.Values.Unbounded.Uncompressed.New_Context,
      Value_Size_Bound    => Types.Values.Unbounded.Uncompressed.Size_Bound,
      Read_Value          => Types.Values.Unbounded.Uncompressed.Read,
      Skip_Value          => Types.Values.Unbounded.Uncompressed.Skip,
      Write_Value         => Types.Values.Unbounded.Uncompressed.Write,
      Parted_Value_Context_Type => Types.Values.Unbounded.Parted.Context_Type,
      New_Parted_Value_Context => Types.Values.Unbounded.Parted.New_Context,
      Parted_Value_Size_Bound => Types.Values.Unbounded.Parted.Size_Bound,
      Fold_Value_Contexts => Types.Values.Unbounded.Parted.Fold_Contexts,
      Value_Context_Size_Bound =>
                             Types.Values.Unbounded.Parted.Context_Size_Bound,
      Read_Value_Context  => Types.Values.Unbounded.Parted.Read_Context,
      Write_Value_Context => Types.Values.Unbounded.Parted.Write_Context,
      Read_Part_Of_Value  => Types.Values.Unbounded.Parted.Read_Part_Of_String,
      Write_Part_Of_Value => Types.Values.Unbounded.Parted.Write_Part_Of_String,
      Is_Context_Free_Serialization =>
              Types.Keys.Is_Context_Free_Serialization and
              Types.Values.Unbounded.Uncompressed.Is_Context_Free_Serialization,
      Block_IO            => Block_IO,
      IO_Buffers          => IO_Buffers);

   ----------
   -- Type wrappers. Always Short (=> BTrees) and not Short (=> Blob_Trees).

   type Map_Type (Short : Boolean := True) is limited
      record
         case Short is
            when True =>  Short_Tree : BTrees.Tree_Type;
            when False => Long_Tree  : Blob_Trees.Tree_Type;
         end case;
      end record;

   type Transaction_Type is abstract tagged limited null record;

   type RO_Transaction_Type (Short : Boolean) is
      new Transaction_Type with
      record
         case Short is
            when True =>  Short_Transaction : BTrees.RO_Transaction_Type;
            when False => Long_Transaction  : Blob_Trees.RO_Transaction_Type;
         end case;
      end record;

   type RW_Transaction_Type (Short : Boolean) is
      new Transaction_Type with
      record
         case Short is
            when True =>  Short_Transaction : BTrees.RW_Transaction_Type;
            when False => Long_Transaction  : Blob_Trees.RW_Transaction_Type;
         end case;
      end record;

   type Bound_Type (Short : Boolean := True) is
      record
         case Short is
            when True =>  Short_Bound : BTrees.Bound_Type;
            when False => Long_Bound : Blob_Trees.Bound_Type;
         end case;
      end record;

   type Cursor_Type (Short : Boolean := True) is limited
      record
         case Short is
            when True =>  Short_Cursor : BTrees.Cursor_Type;
            when False => Long_Cursor  : Blob_Trees.Cursor_Type;
         end case;
      end record;

end DB.Tables.Maps;

