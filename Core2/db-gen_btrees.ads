-- Abstract:
--
-- A generic implementation of [BTree], a B-tree that provides insert, search,
-- deletion and sequential (from lower to tupper) access for variable-length
-- entries.
--
-- Deletions can degenerate the tree, because the do not do any reorganization
-- of the tree. This must be carried out in a batch job or so.
--
-- The great advantage of this implementation is the lack of locks: while for
-- insertion and deletion only local locks are needed (which avoids the
-- bottleneck at the root node), the retrievel doesn't need any locks at all
-- (which predestines this B-tree for distributed IO).
--
-- All operations are process-/thread-safe as long as the Block_IO.Lock and
-- Block_IO.Unlock procedures work as expected.
--
-- This package guarantees that if the entries E_1 .. E_N are the entries of a
-- node, E_I is written before E_{I+1} for all I in 1 .. N.
-- Howevever, entries are read randomly!
-- The Key/Values.Read/Write_Context_Types can be used for compression, for
-- example. In both, the reading and writing case, it is guaranteed that a
-- context object is used only for one node, not more, and either for reading or
-- writing, not both operation types. A context object handed to a
-- Read/Write_Key/Value is either initialized with its default values or has
-- been modified by a previous Read/Write_Key/Value call.
-- Since entries E_1 .. E_N are written in a strictly sequentially, it is
-- guaranteed that the write operation of E_1 is performed with a new context
-- object and the following N - 1 write operations are with the same object.
-- As a consequence, a trivial choice for Key/Values.Read/Write_Context_Types
-- is just null records.
-- (Remind that in this case `written' does not mean that data is necessarily
-- written to disk, it just means serialization of data.)
--
-- Trivial choices for the Skip_Key and Skip_Value operations are mappings to
-- Read_Key and Write_Key, respectively, just without returning the key or
-- value.
--
-- In contrast to normal B+-trees, entries have not a fixed length. An entry may
-- have the size at most (N - M) where N is the size of a node (i.e. Block_Size)
-- and M is the size of the needed meta data.
--
-- References:
--
-- [L&Y] Lehman and Yao -- Efficient locking for concurrent operations on
-- B-trees (http://portal.acm.org/citation.cfm?id=319663)
--
-- Design Notes:
--
-- Exceptions should only be raised under really serious circumstances or in
-- debugging mode.
-- In productive use, the State_Type should be used.
-- XXX Really?
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks;
with DB.Blocks.Gen_IO_Signature;
with DB.Blocks.Gen_Keys_Signature;
with DB.Blocks.Gen_Values_Signature;
with DB.Locks.Mutexes;
with DB.Utils.Gen_Comparisons;

generic
   with package Keys is new Blocks.Gen_Keys_Signature (<>);
   with package Values is new Blocks.Gen_Values_Signature (<>);
   with package Block_IO is new Blocks.Gen_IO_Signature (<>);
   Default_Allow_Duplicates : in Boolean := False;
package DB.Gen_BTrees is
   pragma Preelaborate;

   ----------
   -- Tree initialization procedures.

   type Tree_Type is limited private;

   procedure Create
     (Tree : in out Tree_Type;
      ID   : in     String);
   -- Creates a new tree named ID or raises a DB.IO_Error when creation
   -- fails.

   procedure Create_Temporary
     (Tree : in out Tree_Type;
      ID   : in     String);
   -- Creates a new tree named ID or raises a DB.IO_Error when creation
   -- fails.

   procedure Open
     (Tree : in out Tree_Type;
      ID   : in     String);
   -- Opens Tree with the tree named ID.

   procedure Finalize
     (Tree : in out Tree_Type);
   -- Finalizes Tree, i.e. closes opened files.

   function Max_Key_Size
     (Max_Value_Size : Blocks.Size_Type :=
        Blocks.Bits_To_Units(Values.Value_Type'Size))
      return Blocks.Size_Type;
   -- Returns the maximum allowed size of keys if the maximum size of
   -- values is Max_Value_Size.


   ----------
   -- Core operations: Retrieve, Insertion, Deletion.

   type State_Type is (Success, Failure);

   procedure Search
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type);
   -- Searches the Value associated with Key or sets State = Failure if
   -- no such key exists.
   -- This procedure never blocks because it uses no locks (as long as
   -- Block_IO.Read and Block_IO.Write do not block).

   procedure Search_Minimum
     (Tree  : in out Tree_Type;
      Key   :    out Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type);
   -- Searches the minimum Key / Value pair or sets State = Failure if no
   -- such key exists.
   -- This procedure never blocks because it uses no locks (as long as
   -- Block_IO.Read and Block_IO.Write do not block).

   procedure Insert
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value : in     Values.Value_Type;
      State :    out State_Type);
   -- Synonym for Insert(Tree, Key, Value, Default_Allow_Duplicates, State).
   -- (A default argument for Allow_Duplicates doesn't work well.)

   procedure Insert
     (Tree             : in out Tree_Type;
      Key              : in     Keys.Key_Type;
      Value            : in     Values.Value_Type;
      Allow_Duplicates : in     Boolean;
      State            :    out State_Type);
   -- Inserts a Key / Value pair or sets State = Failure if such a key already
   -- exists.
   -- This procedure might block because of Block_IO.Lock until the lock becomes
   -- available.

   procedure Delete
     (Tree  : in out Tree_Type;
      Key   : in     Keys.Key_Type;
      Value :    out Values.Value_Type;
      State :    out State_Type);
   -- Deletes the Key / Value pair or sets State = Failure if no such key
   -- exists.
   -- This procedure might block because of Block_IO.Lock until the lock becomes
   -- availble.


   ----------
   -- Miscellaneous procedures.

   subtype Count_Type is Long_Integer;

   procedure Count
     (Tree  : in out Tree_Type;
      Count :    out Count_Type);

   procedure Reorganize
     (Tree  : in out Tree_Type;
      State :    out State_Type);
   -- Reorganizes the nodes in the file to avoid degeneration due to deletions.
   -- Not implemented yet.


   ----------
   -- Cursor operations.

   type Cursor_Type is limited private;
   type Bound_Type is private;
   type Comparison_Type is (Less, Less_Or_Equal, Equal, Greater_Or_Equal,
      Greater);

   function Positive_Infinity_Bound
      return Bound_Type;
   -- Returns an abstract bound that means positive infinity.

   function Negative_Infinity_Bound
      return Bound_Type;
   -- Returns an abstract bound that means negative infinity.

   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Keys.Key_Type)
      return Bound_Type;
   -- Creates a concrete bound. The bound New_Bound(Greater, Min) is a
   -- lower bound, because this means that all keys Key hit by the cursor
   -- must satisfy: Key > Min.

   function New_Cursor
     (Tree        : Tree_Type;
      Thread_Safe : Boolean;
      Lower_Bound : Bound_Type;
      Upper_Bound : Bound_Type)
      return Cursor_Type;
   -- Creates a new cursor.
   -- If Thread_Safe is True, all operations on the cursor happen mutually
   -- exclusive.
   -- All key/value-pairs hit by the cursor satisfy Lower_Bound and Upper_Bound.

   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean);
   -- If thread safety is enabled, all operations of the cursor are mutually
   -- exclusive.

   procedure Pause
     (Tree   : in     Tree_Type;
      Cursor : in out Cursor_Type);
   -- Enforces recalibration upon the next operation.

   procedure Next
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Keys.Key_Type;
      Value  :    out Values.Value_Type;
      State  :    out State_Type);
   -- Steps forward to the next Key/Value-pair and sets State to Success.
   -- If no such pair exists (with regard to the set bounds), State is set to
   -- Failure or Error.
   -- If Delete was called before, Next must recalibrate, i.e. find the
   -- key/value-pair that should be visited next.

   procedure Delete
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type;
      Key    :    out Keys.Key_Type;
      Value  :    out Values.Value_Type;
      State  :    out State_Type);
   -- Deletes the Key/Value-pair which was last hit by Cursor and sets State
   -- to the outcome of the deletion. If there was no previous Next call or it
   -- was not successful, the deletion is not successful, either, and State is
   -- set to Failure.
   -- The next Next operation will need to recalibrate, i.e. find the
   -- key/value-pair that should be visited next.

private
   package Key_Comparisons is new Utils.Gen_Comparisons
     (Item_Type => Keys.Key_Type, Compare => Keys.Compare);
   use Key_Comparisons;

   package Nodes is
      RO_Node_Size : constant := Blocks.Block_Size;
      RW_Node_Size : constant := RO_Node_Size * 6 / 4;

      type Degree_Type is range 0 .. Blocks.Block_Size;
      type Level_Type is range 0 .. 255;
      subtype Index_Type is Degree_Type range 0 .. Degree_Type'Last;
      subtype Valid_Index_Type is Index_Type range 1 .. Index_Type'Last;

      type Address_Type is new Block_IO.Address_Type;
      type Valid_Address_Type is new Block_IO.Valid_Address_Type;

      type Node_Type is new Blocks.Base_Block_Type;
      subtype RO_Node_Type is Node_Type(1 .. RO_Node_Size);
      subtype RW_Node_Type is Node_Type(1 .. RW_Node_Size);

      type State_Type is (Valid, Too_Small, Too_Large);
      subtype Validation_State_Type is State_Type;

      Invalid_Index   : constant Index_Type := Index_Type'First;
      Invalid_Address : constant Address_Type :=
         Address_Type(Block_IO.Invalid_Address);
      Leaf_Level      : constant Level_Type := Level_Type'First;

      ----------
      -- Address functions.

      function Is_Valid
        (Address : Address_Type)
         return Boolean;
      -- Returns true if the address is valid.

      function To_Address
        (Address : Valid_Address_Type)
         return Address_Type;
      -- Converts the given valid address to a general one.

      function To_Valid_Address
        (Address : Address_Type)
         return Valid_Address_Type;
      -- Converts the given address to a valid one.

      function Is_Valid
        (Index : Index_Type)
         return Boolean;
      -- Returns true if the given index is inside Valid_Index_Type range.

      ----------
      -- General and accessor subprograms.

      function Root_Node
        (Level : Level_Type)
         return RW_Node_Type;
      -- Returns a simple root node of degree 0.

      function Is_Leaf
        (Node : Node_Type)
         return Boolean;
      -- Indicates whether the given node is a leaf. Leaves have (Key, Value)
      -- entries.

      function Is_Inner
        (Node : Node_Type)
         return Boolean;
      -- Indicates whether the given node is a leaf. Leaves have
      -- (Key, Child, Count) entries.

      function Level
        (Node : Node_Type)
         return Level_Type;
      -- Returns the level of the node. The level of leaves is zero, the level
      -- of the parent's of leaves is 1 etc.

      function Degree
        (Node : Node_Type)
         return Degree_Type;
      -- Returns the degree of the node. The degree is the count of keys,
      -- children, counts and/or values.

      function Link
        (Node : Node_Type)
         return Address_Type;
      -- Returns the address of the right neighbor of the node.

      function Valid_Link
        (Node : Node_Type)
         return Valid_Address_Type;
      -- Returns the address of the right neighbor of the node.

      procedure Get_High_Key
        (Node     : in  Node_Type;
         High_Key : out Keys.Key_Type;
         Success  : out Boolean);
      -- Gets the high key of Node. If Node is empty, the high key is the key
      -- lastly deleted from Node; if Node never contained a value, Success is
      -- set to False. Otherwise, if Node is not empty, the high is just the
      -- greatest key.

      function Has_High_Key
        (Node : Nodes.Node_Type)
         return Boolean;
      -- Returns True iff Node has a high key.

      function High_Key
        (Node : Nodes.Node_Type)
         return Keys.Key_Type;
      -- Returns the high key of Node if it exists. Otherwise Tree_Error is
      -- raised.

      procedure Get_Key
        (Node        : in     Node_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Keys.Key_Type;
         Key_Context : in out Keys.Read_Context_Type);
      -- Gets the Index-th key. This function is defined for both,
      -- leaves and inner nodes.

      function Key
        (Node  : Node_Type;
         Index : Valid_Index_Type)
         return Keys.Key_Type;
      -- Returns the Index-th key. This function is defined for both,
      -- leaves and inner nodes.

      procedure Get_Child
        (Node        : in     Node_Type;
         Index       : in     Valid_Index_Type;
         Child       :    out Valid_Address_Type;
         Key_Context : in out Keys.Read_Context_Type);
      -- Gets the Index-th child address. This function is defined for
      -- inner nodes only.

      function Child
        (Node  : Node_Type;
         Index : Valid_Index_Type)
         return Valid_Address_Type;
      -- Returns the Index-th child address. This function is defined for
      -- inner nodes only.

      procedure Get_Value
        (Node          : in     Node_Type;
         Index         : in     Valid_Index_Type;
         Value         :    out Values.Value_Type;
         Key_Context   : in out Keys.Read_Context_Type;
         Value_Context : in out Values.Read_Context_Type);
      -- Returns the Index-th value. This function is defined for leaves.

      function Value
        (Node  : Node_Type;
         Index : Valid_Index_Type)
         return Values.Value_Type;
      -- Returns the Index-th value. This function is defined for leaves.

      function Key_Position
        (Node : Node_Type;
         Key  : Keys.Key_Type)
         return Index_Type;
      -- Returns the (first) position of the given key or returns Invalid_Index.

      function Child_Position
        (Node  : Node_Type;
         Child : Valid_Address_Type)
         return Index_Type;
      -- Returns the position of the given child or returns Invalid_Index.

      --------- -
      -- Node operations.

      procedure Set_Link
        (Node     : in out RW_Node_Type;
         Neighbor : in     Address_Type);
      -- Sets the address of the right neighbor of the node.

      procedure Set_Link
        (Node     : in out RW_Node_Type;
         Neighbor : in     Valid_Address_Type);
      -- Sets the address of the right neighbor of the node.

      function Split_Position
        (Node : RW_Node_Type)
         return Valid_Index_Type;
      -- Returns the position for a split. This position is the index of the
      -- first entry which should be (the first) member of the right node after
      -- the split.

      function Insertion
        (Node  : RW_Node_Type;
         Index : Valid_Index_Type;
         Key   : Keys.Key_Type;
         Child : Valid_Address_Type)
         return RW_Node_Type;
      -- Returns the node that results from the insertion of (Key, Child,
      -- Count) at position Index. This function is defined for inner nodes.

      function Insertion
        (Node  : RW_Node_Type;
         Index : Valid_Index_Type;
         Key   : Keys.Key_Type;
         Value : Values.Value_Type)
         return RW_Node_Type;
      -- Returns the node that results from the substitution of (Key, Value) at
      -- position Index. This function is defined for leaves.

      function Substitution
        (Node  : RW_Node_Type;
         Index : Valid_Index_Type;
         Key   : Keys.Key_Type;
         Child : Valid_Address_Type)
         return RW_Node_Type;
      -- Returns the node that results from the substitution of (Key, Child,
      -- Count) at position Index. This function is defined for inner nodes.

      function Substitution
        (Node  : RW_Node_Type;
         Index : Valid_Index_Type;
         Key   : Keys.Key_Type;
         Value : Values.Value_Type)
         return RW_Node_Type;
      -- Returns the node that results from the insertion of (Key, Value) at
      -- position Index. This function is defined for leaves.

      function Deletion
        (Node  : RW_Node_Type;
         Index : Valid_Index_Type)
         return RW_Node_Type;
      -- Returns the node that results from the deletion of the Index-th child.
      -- This function works for both, leaves and inner nodes.

      function Copy
        (Node : RW_Node_Type;
         From : Valid_Index_Type;
         To   : Index_Type)
         return RW_Node_Type;
      -- Returns a copy of the node which is trimmed to the entries From .. To.
      -- This function works for both, leaves and inner nodes.

      function Combination
        (Left_Node  : RW_Node_Type;
         Right_Node : RW_Node_Type)
         return RW_Node_Type;
      -- Returns a combination of Left_Node and Right_Node. The degree is the
      -- sum of both, the left neighbor is Left_Node's and the right neighbor is
      -- Right_Node's one. The nodes must be either both leaves or both
      -- inner nodes.

      ----------
      -- Block-related subprograms.

      function Size_Of
        (Node : Node_Type)
         return Blocks.Size_Type;
      -- Returns the size used in the node.

      function Is_Safe
        (Node    : Node_Type;
         Is_Root : Boolean := False)
         return Boolean;
      -- Checks whether the given node is valid, i.e. whether it fits into a
      -- single disk block.

      function Validation
        (Node    : Node_Type;
         Is_Root : Boolean := False)
         return Validation_State_Type;
      -- Checks whether the given node is valid, i.e. whether it fits into a
      -- single disk block.

      function Max_Key_Size
        (Max_Value_Size : Blocks.Size_Type)
         return Blocks.Size_Type;

      function To_Block
        (Node : Node_Type)
         return Blocks.Block_Type;
      -- Converts a node to a block. Sets unused bytes to zeros.

   private
      pragma Inline (Root_Node);
      pragma Inline (Is_Valid);
      pragma Inline (Is_Leaf);
      pragma Inline (Is_Inner);
      pragma Inline (Level);
      pragma Inline (Degree);
      pragma Inline (Set_Link);
      pragma Inline (Link);
      pragma Inline (Valid_Link);
      pragma Inline (Key);
      pragma Inline (Child);
      pragma Inline (Value);
      pragma Inline (Key_Position);
      pragma Inline (Child_Position);
      pragma Inline (Split_Position);
      pragma Inline (Is_Valid);
      pragma Inline (To_Valid_Address);
      pragma Inline (To_Address);
      pragma Inline (To_Block);
   end Nodes;


   ----------
   -- Tree type.

   Invalid_Address : constant Nodes.Address_Type :=
      Nodes.Address_Type(Block_IO.Invalid_Address);

   Root_Address : constant Nodes.Valid_Address_Type :=
      Nodes.Valid_Address_Type(Block_IO.First);

   type Tree_Ref_Type is not null access all Tree_Type;
   pragma Controlled (Tree_Ref_Type);
   for Tree_Ref_Type'Storage_Size use 0;

   type Tree_Type is limited
      record
         File        : Block_IO.File_Type;
         Self        : Tree_Ref_Type := Tree_Type'Unchecked_Access;
         Initialized : Boolean       := False;
         Finalized   : Boolean       := False;
      end record;

   procedure Read_Node
     (Tree        : in out Tree_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           :    out Nodes.Node_Type);

   procedure Write_Node
     (Tree : in out Tree_Type;
      N_A  : in     Nodes.Valid_Address_Type;
      N    : in     Nodes.RW_Node_Type);

   procedure Write_New_Node
     (Tree : in out Tree_Type;
      N_A  :    out Nodes.Valid_Address_Type;
      N    : in     Nodes.RW_Node_Type);


   ----------
   -- Cursor types.

   type Bound_Kind_Type is (Concrete_Bound, Abstract_Bound);
   type Infinity_Type is (Negative_Infinity, Positive_Infinity);
   type Bound_Type (Kind : Bound_Kind_Type := Concrete_Bound) is
      record
         case Kind is
            when Concrete_Bound =>
               Comparison : Comparison_Type;
               Key        : Keys.Key_Type;
            when Abstract_Bound =>
               Location   : Infinity_Type;
         end case;
      end record;

   type Cursor_Type is limited
      record
         Final              : Boolean := False;
         Lower_Bound        : Bound_Type;
         Upper_Bound        : Bound_Type;
         Has_Node           : Boolean := False;
         Node               : Nodes.RO_Node_Type;
         Key_Context        : Keys.Read_Context_Type;
         Value_Context      : Values.Read_Context_Type;
         Index              : Nodes.Valid_Index_Type;
         Key                : Keys.Key_Type;
         Force_Recalibrate  : Boolean := False;
         Owning_Tree        : Tree_Ref_Type;
         Initialized        : Boolean := False;
         Thread_Safe        : Boolean;
         Mutex              : Locks.Mutexes.Mutex_Type;
      end record;

end DB.Gen_BTrees;

