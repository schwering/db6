-- Abstract:
--
-- Column families store a number of columns physically together. 
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Finalization;
with GNAT.Regexp;

with DB.IO.Blocks;
with DB.Tables.Maps;
with DB.Types.Keys;

package DB.Tables.Column_Families is
   pragma Elaborate_Body;

   ----------
   -- Column_Family initialization operations.

   type Column_Family_Type is
      new Ada.Finalization.Limited_Controlled with private;

   function New_Column_Family
     (Regexp         : in String;
      Max_Key_Size   : in IO.Blocks.Size_Type;
      Max_Value_Size : in IO.Blocks.Size_Type)
      return Column_Family_Type;
   -- Initializes a Column_Family object.

   function Matches
     (Column_Family : Column_Family_Type;
      Column_Name   : String)
      return Boolean;
   -- Indicates whether Column_Family is to store records with the respective
   -- Column_Name.

   function Matches
     (Column_Family : Column_Family_Type;
      Column_Name   : Types.Keys.Columns.String_Type)
      return Boolean;
   -- Indicates whether Column_Family is to store records with the respective
   -- Column_Name.

   procedure Create
     (ID             : in String;
      Max_Key_Size   : in IO.Blocks.Size_Type;
      Max_Value_Size : in IO.Blocks.Size_Type);
   -- Creates a new Column_Family named ID or raises a DB.IO.IO_Error when
   -- creation fails.

   procedure Initialize
     (Column_Family : out Column_Family_Type;
      ID            : in  String);
   -- Initializes Column_Family with the Column_Family named ID.

   overriding
   procedure Finalize
     (Column_Family : in out Column_Family_Type);
   -- Finalizes Column_Family, i.e. closes opened files.

   function Max_Key_Size
     (Column_Family  : Column_Family_Type;
      Max_Value_Size : IO.Blocks.Size_Type)
      return IO.Blocks.Size_Type;
   -- Returns the maximum allowed size of keys.

   ----------
   -- Transactions and their operations.

   subtype Transaction_Type is Maps.Transaction_Type;
   subtype RO_Transaction_Type is Maps.RO_Transaction_Type;
   subtype RW_Transaction_Type is Maps.RW_Transaction_Type;

   function New_RO_Transaction
     (Column_Family : Column_Family_Type)
      return RO_Transaction_Type;

   procedure Start_Transaction
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RO_Transaction_Type);

   procedure Finish_Transaction
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RO_Transaction_Type);

   function New_RW_Transaction
     (Column_Family  : Column_Family_Type)
      return RW_Transaction_Type;

   procedure Start_Transaction
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type);

   procedure Abort_Transaction
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type);

   procedure Commit_Transaction
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type);

   ----------
   -- Core operations: Retrieve, Insertion, Deletion.

   subtype State_Type is Maps.State_Type;
   procedure Retrieve
     (Column_Family : in out Column_Family_Type;
      Key           : in     Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type);
   -- Searches the Value associated with Key or sets State = Failure if
   -- no such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Retrieve
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Key           : in     Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type);
   -- Searches the Value associated with Key or sets State = Failure if
   -- no such key exists.

   procedure Minimum
     (Column_Family : in out Column_Family_Type;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type);
   -- Searches the minimum Key / Value pair or sets State = Failure if no
   -- such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Minimum
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type);
   -- Searches the minimum Key / Value pair or sets State = Failure if no
   -- such key exists.

   procedure Maximum
     (Column_Family : in out Column_Family_Type;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type);
   -- Searches the maximum Key / Value pair or sets State = Failure if no
   -- such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Maximum
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type);
   -- Searches the maximum Key / Value pair or sets State = Failure if no
   -- such key exists.

   procedure Insert
     (Column_Family : in out Column_Family_Type;
      Key           : in     Key_Type;
      Value         : in     Value_Type'Class;
      State         :    out State_Type);
   -- Inserts a Key / Value pair or sets State = Failure if such a key already
   -- exists.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Insert
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type'Class;
      Key           : in     Key_Type;
      Value         : in     Value_Type'Class;
      State         :    out State_Type);
   -- Inserts a Key / Value pair or sets State = Failure if such a key already
   -- exists.

   procedure Delete
     (Column_Family : in out Column_Family_Type;
      Key           : in     Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type);
   -- Deletes the Key / Value pair or sets State = Failure if no such key
   -- exists.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Delete
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type'Class;
      Key           : in     Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type);
   -- Deletes the Key / Value pair or sets State = Failure if no such key
   -- exists.

   ----------
   -- Miscellaneous information procedures.

   subtype Count_Type is Maps.Count_Type;

   procedure Count
     (Column_Family : in out Column_Family_Type;
      Count         :    out Count_Type);
   -- Determines the count N of key / value pairs in the Column_Family.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Count
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Count         :    out Count_Type);
   -- Determines the count N of key / value pairs in the Column_Family.

   procedure Get_Height
     (Column_Family : in out Column_Family_Type;
      Height        :    out Natural);
   -- Determines the Height of the Column_Family.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Get_Height
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Height        :    out Natural);
   -- Determines the Height of the Column_Family.

   procedure Clusterize
     (Column_Family : in out Column_Family_Type;
      State         :    out State_Type);
   -- Reorganizes the nodes in the file.
   -- Not implemented yet.

   ----------
   -- Cursor operations.

   subtype Comparison_Type is Maps.Comparison_Type;
   subtype Bound_Type is Maps.Bound_Type;
   subtype Cursor_Type is Maps.Cursor_Type;


   function Positive_Infinity_Bound
     (Column_Family : Column_Family_Type)
      return Bound_Type;
   -- Returns an abstract bound that means positive infinity.

   function Negative_Infinity_Bound
     (Column_Family : Column_Family_Type)
      return Bound_Type;
   -- Returns an abstract bound that means negative infinity.

   function New_Bound
     (Column_Family : Column_Family_Type;
      Comparison    : Comparison_Type;
      Key           : Key_Type)
      return Bound_Type;
   -- Creates a concrete bound. The bound New_Bound(Greater, Min) is a
   -- lower bound, because this means that all keys Key hit by the cursor
   -- must satisfy: Key > Min.

   function New_Cursor
     (Column_Family     : Column_Family_Type;
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
     (Column_Family : in     Column_Family_Type;
      Transaction   : in     Transaction_Type'Class;
      Cursor        : in out Cursor_Type);
   -- Releases all resources hold be the cursor. In particular, this is a
   -- small amount of memory (the cursors use the heap; it would be possible
   -- without the heap, but probably not faster) and, in case of the
   -- non-transactional cursors, the unlock of the locks and release of the
   -- ticket.

   procedure Pause
     (Column_Family : in out Column_Family_Type;
      Cursor        : in out Cursor_Type);
   -- Releases the lock hold by Cursor. The next Next or Unpause call will
   -- acquire the needed read-lock again. Furthermore, the next Next call will
   -- try to recalibrate the Cursor, i.e. find the key/value-pair that should
   -- be visited next. Hence, it is allowed to delete elements from
   -- Column_Family
   -- in between two Pause and Unpause calls.

   procedure Unpause
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Cursor        : in out Cursor_Type);
   -- Acquires the needed read-lock again after a Pause call.

   procedure Next
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out Transaction_Type'Class;
      Cursor        : in out Cursor_Type;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type);
   -- Steps forward to the next Key/Value-pair and sets State to Success.
   -- If no such pair exists (with regard to the set bounds), State is set to
   -- Failure or Error.
   -- If Delete was called before, Next must recalibrate, i.e. find the
   -- key/value-pair that should be visited next.

   procedure Delete
     (Column_Family : in out Column_Family_Type;
      Transaction   : in out RW_Transaction_Type'Class;
      Cursor        : in out Cursor_Type;
      Key           :    out Key_Type;
      Value         :    out Value_Type'Class;
      State         :    out State_Type);
   -- Deletes the Key/Value-pair which was last hit by Cursor and sets State
   -- to the outcome of the deletion. If there was no previous Next call or it
   -- was not successful, the deletion is not successful, either, and State is
   -- set to Failure.
   -- The next Next operation will need to recalibrate, i.e. find the
   -- key/value-pair that should be visited next.

private
   type Map_Ref_Type is access Maps.Map_Type;

   type Column_Family_Type is new Ada.Finalization.Limited_Controlled with
      record
         Guard : GNAT.Regexp.Regexp;
         Map   : Map_Ref_Type := null;
      end record;

end DB.Tables.Column_Families;

