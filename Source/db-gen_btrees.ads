with System.Storage_Pools;
with System.Pool_Global;

with DB.IO.Blocks;
with DB.IO.Blocks.Gen_Buffers;
with DB.IO.Blocks.Gen_IO;
with DB.Locks.Mutexes;

generic
   type Key_Type is private;
   type Key_Context_Type is limited private;
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

   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   with function "<=" (Left, Right : Key_Type) return Boolean is <>;

   type Value_Type is private;
   type Value_Context_Type is limited private;
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

   Is_Context_Free_Serialization : in Boolean;

   with package Block_IO is new IO.Blocks.Gen_IO (<>);
package DB.Gen_BTrees is
   --pragma Preelaborate;
   pragma Unreferenced (Skip_Value);

   type Tree_Type is limited private;
   type Result_Type is (Success, Failure, Error);
   type Count_Type is new Natural;
   subtype Height_Type is Positive;
   type Transaction_Type is abstract tagged limited private;
   type RO_Transaction_Type is new Transaction_Type with private;
   type RW_Transaction_Type is new Transaction_Type with private;

   type Comparison_Type is (Less, Less_Or_Equal, Equal, Greater_Or_Equal, 
      Greater);
   type Bound_Type (<>) is private;
   type Cursor_Type (<>) is limited private;


   Tree_Error : exception;
   -- This exception is only raised when there are extremely serious 
   -- errors in the tree such as dangling references to child or neighbor
   -- nodes.


   -- Tree initialization procedures: Create, Initialize, Finalize.

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
     (Max_Value_Size : IO.Blocks.Size_Type
                     := IO.Blocks.Bits_To_Units(Value_Type'Size))
      return IO.Blocks.Size_Type;
   -- Returns the maximum allowed size of keys if the maximum size of 
   -- values is Max_Value_Size.


   -- Transaction: New_Transaction, Start_Transaction, Abort_Transaction,
   -- Commit_Transaction.

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


   -- Search operations: Look_Up, Minimum, Maximum.

   procedure Look_Up
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);
   -- Searches the Value associated with Key or sets State = Failure if
   -- no such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Look_Up
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);
   -- Searches the Value associated with Key or sets State = Failure if
   -- no such key exists.

   procedure Look_Up
     (Tree     : in out Tree_Type;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type);
   -- Searches the Position-th Key / Value pair or sets State = Failure if
   -- no such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Look_Up
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type);
   -- Searches the Position-th Key / Value pair or sets State = Failure if
   -- no such key exists.

   procedure Minimum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);
   -- Searches the minimum Key / Value pair or sets State = Failure if no
   -- such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Minimum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);
   -- Searches the minimum Key / Value pair or sets State = Failure if no
   -- such key exists.

   procedure Maximum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);
   -- Searches the maximum Key / Value pair or sets State = Failure if no
   -- such key exists.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Maximum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);
   -- Searches the maximum Key / Value pair or sets State = Failure if no
   -- such key exists.


   -- Modification procedures: Insert, Delete.

   procedure Insert
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);
   -- Inserts a Key / Value pair or sets State = Failure if such a key already
   -- exists.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.
 
   procedure Insert
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);
   -- Inserts a Key / Value pair or sets State = Failure if such a key already
   -- exists.

   procedure Delete
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type);
   -- Deletes the Key / Value pair or sets State = Failure if no such key
   -- exists.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type);
   -- Deletes the Key / Value pair or sets State = Failure if no such key
   -- exists.

   procedure Delete
     (Tree     : in out Tree_Type;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type);
   -- Deletes the Position-th Key / Value pair or sets State = Failure if no
   -- such key exists.
   -- This procedure starts and commits a new transaction and might therefore
   -- block.

   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type);
   -- Deletes the Position-th Key / Value pair or sets State = Failure if no
   -- such key exists.


   -- Information procedures and other utilities: Count, Get_Height, Clusterize.

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
      Height :    out Height_Type);
   -- Determines the Height of the tree.
   -- This procedure acquires a read-lock and might therefore block due to
   -- uncommitted transactions.

   procedure Get_Height
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Height_Type);
   -- Determines the Height of the tree.

   procedure Clusterize
     (Tree  : in out Tree_Type;
      State :    out Result_Type);
   -- Reorganizes the nodes in the file.
   -- Not implemented yet.


   -- Cursor.

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
   -- Creates a new cursor. In contrast to the other New_Cursor constructor,
   -- cursors created by this one do not work with an own ticket and locks;
   -- instead, it relies on a Transaction. This Transaction must be initialized,
   -- of course.
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

   procedure Finalize
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type);
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
   -- Only valid for non-transactional cursors.

   procedure Unpause
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type);
   -- Acquires the needed read-lock again after a Pause call.
   -- Only valid for non-transactional cursors.

   procedure Next
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out Result_Type);
   -- Steps forward to the next Key/Value-pair and sets State to Success.
   -- If no such pair exists (with regard to the set bounds), State is set to 
   -- Failure or Error.
   -- If Delete was called before, Next must recalibrate, i.e. find the
   -- key/value-pair that should be visited next.
   -- Only valid for transactional cursors.

   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      State       :    out Result_Type);
   -- Deletes the Key/Value-pair which was last hit by Cursor and sets State
   -- to the outcome of the deletion. If there was no previous Next call or it
   -- was not successful, the deletion is not successful, either, and State is
   -- set to Failure.
   -- The next Next operation will need to recalibrate, i.e. find the
   -- key/value-pair that should be visited next.
   -- The deletion involves the start and commit of a sub-transaction, hence
   -- the lock hold by the ticket of the Cursor is upgraded to a write-lock
   -- and then a certify-lock for a short time. 
   -- Note that this might lead to deadlocks or starvation if two cursors
   -- simultaneously iterate over the elements of a Tree and perform deletions.
   -- Only valid for non-transactional cursors.

   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      State       :    out Result_Type);
   -- Deletes the Key/Value-pair which was last hit by Cursor and sets State
   -- to the outcome of the deletion. If there was no previous Next call or it
   -- was not successful, the deletion is not successful, either, and State is
   -- set to Failure.
   -- The next Next operation will need to recalibrate, i.e. find the
   -- key/value-pair that should be visited next.
   -- Only valid for transactional cursors.

private
   package Nodes is
      type Degree_Type is range 0 .. IO.Blocks.Block_Size;
      subtype Index_Type is Degree_Type range 0 .. Degree_Type'Last;
      subtype Valid_Index_Type is Index_Type range 1 .. Index_Type'Last;
      type Address_Type is new Block_IO.Address_Type;
      type Valid_Address_Type is new Block_IO.Valid_Address_Type;

      type Node_Type is private;

      type State_Type is (Valid, Too_Small, Too_Large);
      subtype Validation_Result_Type is State_Type;

      Invalid_Index   : constant Index_Type
                      := Index_Type'First;
      Invalid_Address : constant Address_Type
                      := Address_Type(Block_IO.Invalid_Address);


      -- General and accessor subprograms

      function Root_Node
        (Is_Leaf : Boolean)
         return Node_Type;
      -- Returns a simple root node of degree 0.

      function Free_Node
         return Node_Type;
      -- Returns a free node.

      function Is_Free
        (Node : Node_Type)
         return Boolean;
      -- Indicates whether the given node is a free one (which does not contain
      -- any valuable information) or an active one.

      function Degree
        (Node : Node_Type)
         return Degree_Type;
      -- Returns the degree of the node. The degree is the count of keys,
      -- children, counts and/or values.

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

      procedure Set_Parent
        (Node   : in out Node_Type;
         Parent : in     Address_Type);
      -- Sets the address of the parent of the node. To mark the node as root
      -- the address should be Invalid_Address.

      procedure Set_Parent
        (Node   : in out Node_Type;
         Parent : in     Valid_Address_Type);
      -- Sets the address of the parent of the node. To mark the node as root
      -- the address should be Invalid_Address.

      function Parent
        (Node : Node_Type)
         return Address_Type;
      -- Returns the address of the parent of the node. Is Invalid_Address if
      -- the node is the root.

      function Valid_Parent
        (Node : Node_Type)
         return Valid_Address_Type;
      -- Returns the address of the parent of the node.

      function Is_Root
        (Node : Node_Type)
         return Boolean;
      -- Indicates whether the given node is the root. This is the case if the
      -- left and right neighbor addresses are both invalid. (The parent address
      -- is not considered!)

      procedure Set_Left_Neighbor
        (Node     : in out Node_Type;
         Neighbor : in     Valid_Address_Type);
      -- Sets the address of the left neighbor of the node.

      procedure Set_Left_Neighbor
        (Node     : in out Node_Type;
         Neighbor : in     Address_Type);
      -- Sets the address of the left neighbor of the node.

      function Left_Neighbor
        (Node : Node_Type)
         return Address_Type;
      -- Returns the address of the left neighbor of the node.

      function Valid_Left_Neighbor
        (Node : Node_Type)
         return Valid_Address_Type;
      -- Returns the address of the left neighbor of the node.

      procedure Set_Right_Neighbor
        (Node     : in out Node_Type;
         Neighbor : in     Address_Type);
      -- Sets the address of the right neighbor of the node.

      procedure Set_Right_Neighbor
        (Node     : in out Node_Type;
         Neighbor : in     Valid_Address_Type);
      -- Sets the address of the right neighbor of the node.

      function Right_Neighbor
        (Node : Node_Type)
         return Address_Type;
      -- Returns the address of the right neighbor of the node.

      function Valid_Right_Neighbor
        (Node : Node_Type)
         return Valid_Address_Type;
      -- Returns the address of the right neighbor of the node.

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

      function Key
        (Node  : Node_Type;
         Index : Valid_Index_Type)
         return Key_Type;
      -- Returns the Index-th value. This function is determined for both,
      -- leaves and inner nodes.

      function Child
        (Node  : Node_Type;
         Index : Valid_Index_Type)
         return Valid_Address_Type;
      -- Returns the Index-th value. This function is determined for inner nodes
      -- only.

      function Count
        (Node  : Node_Type;
         Index : Valid_Index_Type)
         return Count_Type;
      -- Returns the Index-th value. This function is determined for inner nodes
      -- only.

      function Count_Sum
        (Node : Node_Type)
         return Count_Type;
      -- Sums all counts in the node or, if the node is a leaf, simply returns
      -- the degree. This function is determined for both, leaves and inner
      -- nodes.

      function Count_Sum
        (Node     : Node_Type;
         To_Index : Index_Type)
         return Count_Type;
      -- Sums all counts in the node up to the given index (exclusive).
      -- This function is determined for both, leaves and inner nodes.

      function Value
        (Node  : Node_Type;
         Index : Valid_Index_Type)
         return Value_Type;
      -- Returns the Index-th value. This function is determined for leaves.


      -- Node operations

      function Key_Position
        (Node : Node_Type;
         Key  : Key_Type)
         return Index_Type;
      -- Returns the (first) position of the given key.

      function Count_Position
        (Node      : Node_Type;
         Count_Sum : Count_Type)
         return Index_Type;
      -- Returns the position of the subtree that contains the Counth-th child.

      function Child_Position
        (Node  : Node_Type;
         Child : Valid_Address_Type)
         return Valid_Index_Type;
      -- Returns the position of the given child. If the child is not contained
      -- in the node, a Node_Error is raised.

      function Split_Position
        (Node : Node_Type)
         return Valid_Index_Type;
      -- Returns the position for a split. This position is the index of the
      -- first entry which should be (the first) member of the right node after
      -- the split.

      function Combi_Split_Position
        (Left_Node  : Node_Type;
         Right_Node : Node_Type)
         return Valid_Index_Type;
      -- Returns the position for a split. This position is the index of the
      -- first entry which should be (the first) member of the right node after
      -- the split. If the returned index is in 1 .. Degree(Left_Node), it
      -- denotes an element of Left_Node, if the index is in
      -- Degree(Left_Node) + 1 .. Degree(Left_Node) + Degree(Right_Node), it
      -- denotes an element of Right_Node.

      -- Returns true if the given index is inside Valid_Index_Type range.
      function Is_Valid
        (Index : Index_Type)
         return Boolean;

      function Insertion
        (Node  : Node_Type;
         Index : Valid_Index_Type;
         Key   : Key_Type;
         Child : Valid_Address_Type;
         Count : Count_Type)
         return Node_Type;
      -- Returns the node that results from the insertion of (Key, Child,
      -- Count) at position Index. This function is determined for inner nodes.

      function Insertion
        (Node  : Node_Type;
         Index : Valid_Index_Type;
         Key   : Key_Type;
         Value : Value_Type)
         return Node_Type;
      -- Returns the node that results from the substitution of (Key, Value) at
      -- position Index. This function is determined for leaves.

      function Substitution
        (Node  : Node_Type;
         Index : Valid_Index_Type;
         Key   : Key_Type;
         Child : Valid_Address_Type;
         Count : Count_Type)
         return Node_Type;
      -- Returns the node that results from the substitution of (Key, Child,
      -- Count) at position Index. This function is determined for inner nodes.

      function Substitution
        (Node  : Node_Type;
         Index : Valid_Index_Type;
         Key   : Key_Type;
         Value : Value_Type)
         return Node_Type;
      -- Returns the node that results from the insertion of (Key, Value) at
      -- position Index. This function is determined for leaves.

      procedure Set_Count
        (Node  : in out Node_Type;
         Index : in     Valid_Index_Type;
         Count : in     Count_Type);
      -- Updates the count at position Index. This function works for inner
      -- nodes.

      procedure Set_Child_And_Count
        (Node  : in out Node_Type;
         Index : in     Valid_Index_Type;
         Child : in     Valid_Address_Type;
         Count : in     Count_Type);
      -- Updates the child address and count at position Index. This function
      -- works for inner nodes.

      function Deletion
        (Node  : Node_Type;
         Index : Valid_Index_Type)
         return Node_Type;
      -- Returns the node that results from the deletion of the Index-th child.
      -- This function works for both, leaves and inner nodes.

      function Copy
        (Node : Node_Type;
         From : Valid_Index_Type;
         To   : Index_Type)
         return Node_Type;
      -- Returns a copy of the node which is trimmed to the entries From .. To.
      -- This function works for both, leaves and inner nodes.

      function Combi_Copy
        (Left_Node  : Node_Type;
         Right_Node : Node_Type;
         From       : Valid_Index_Type;
         To         : Index_Type)
         return Node_Type;
      -- Returns a copy of the nodes which is trimmed to the entries From .. To.
      -- For both indexes it holds that the index refers to Left_Node if it is
      -- 1 .. Degree(Left_Node) and to Right_Node if it is in 
      -- Degree(Left_Node) + 1 .. Degree(Left_Node) + Degree(Right_Node).
      -- This function works for both, leaves and inner nodes.

      function Combination
        (Left_Node  : Node_Type;
         Right_Node : Node_Type)
         return Node_Type;
      -- Returns a combination of Left_Node and Right_Node. The degree is the
      -- sum of both, the left neighbor is Left_Node's and the right neighbor is
      -- Right_Node's one. The parent is Right_Node's parent, because in most
      -- cases, the new node should be copied into the place of Right_Node.
      -- The nodes must not be free. They must be either both leaves or both
      -- inner nodes.


      -- Block-related subprograms

      function Size_Of
        (Node : Node_Type)
         return IO.Blocks.Size_Type;
      -- Returns the size used in the node.

      function Is_Valid
        (Node           : Node_Type;
         Force_Non_Root : Boolean := False)
         return Boolean;
      -- Checks whether the given node is valid, i.e. whether it fits into a
      -- single disk block.

      function Validation
        (Node           : Node_Type;
         Force_Non_Root : Boolean := False)
         return Validation_Result_Type;
      -- Checks whether the given node is valid, i.e. whether it fits into a
      -- single disk block.

      function Max_Key_Size
        (Max_Value_Size : IO.Blocks.Size_Type)
         return IO.Blocks.Size_Type;

      function To_Block
        (Node : Node_Type)
         return IO.Blocks.Block_Type;
      -- Converts a node to a block.

      function From_Block
        (Block : IO.Blocks.Block_Type)
         return Node_Type;
      -- Converts a block to a node.

      Node_Error : exception renames Tree_Error;

   private
      type Node_Type is
         record
            Block     : IO.Blocks.Long_Block_Type;
            Ok        : Boolean;
         end record;

      pragma Inline (Root_Node);
      pragma Inline (Is_Free);
      pragma Inline (Degree);
      pragma Inline (Is_Leaf);
      pragma Inline (Is_Inner);
      pragma Inline (Set_Parent);
      pragma Inline (Parent);
      pragma Inline (Is_Root);
      pragma Inline (Set_Left_Neighbor);
      pragma Inline (Left_Neighbor);
      pragma Inline (Set_Right_Neighbor);
      pragma Inline (Right_Neighbor);
      pragma Inline (Key);
      pragma Inline (Child);
      pragma Inline (Count);
      pragma Inline (Count_Sum);
      pragma Inline (Value);
      pragma Inline (Key_Position);
      pragma Inline (Child_Position);
      pragma Inline (Split_Position);
      pragma Inline (Is_Valid);
      pragma Inline (Set_Count);
      pragma Inline (Set_Child_And_Count);
      pragma Inline (To_Valid_Address);
      pragma Inline (To_Address);
      pragma Inline (To_Block);
      pragma Inline (From_Block);
   end Nodes;


   Root_Address : constant Nodes.Valid_Address_Type
                := Nodes.Valid_Address_Type(Block_IO.First);
   Free_Address : constant Nodes.Valid_Address_Type
                := Nodes.Valid_Address_Type(Block_IO.Succ(Block_IO.First));

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


   type Transaction_Ref_Type is not null access all Transaction_Type'Class;
   pragma Controlled (Transaction_Ref_Type);
   for Transaction_Ref_Type'Storage_Size use 0;

   type Transaction_Type is abstract tagged limited
      record
         Current_Root_Address : Nodes.Valid_Address_Type := Root_Address;
         Ticket               : Block_IO.Ticket_Type;
         Initialized          : Boolean := False;
         Started              : Boolean := False;
         Owning_Tree          : Tree_Ref_Type;
         Self                 : Transaction_Ref_Type
                              := Transaction_Type'Unchecked_Access;
      end record;

   procedure Read_Node
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           :    out Nodes.Node_Type);
   -- Would be abstract if this wouldn't force Write_Node being visible.
   -- Therefore, it simply raises an exception.


   type RO_Transaction_Type is new Transaction_Type with null record;

   overriding
   procedure Read_Node
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           :    out Nodes.Node_Type);


   type RW_Transaction_Ref_Type is
      not null access all RW_Transaction_Type'Class;
   pragma Controlled (RW_Transaction_Ref_Type);
   for RW_Transaction_Ref_Type'Storage_Size use 0;

   package IO_Buffers is new IO.Blocks.Gen_Buffers
     (Block_IO          => Block_IO,
      Item_Type         => Nodes.Node_Type,
      To_Block          => Nodes.To_Block,
      From_Block        => Nodes.From_Block,
      Item_Storage_Pool => Tree_Ref_Type'Storage_Pool,
      Node_Storage_Pool => Tree_Ref_Type'Storage_Pool);

   type RW_Transaction_Type is new Transaction_Type with
      record
         Buffer  : IO_Buffers.Buffer_Type;
         RW_Self : RW_Transaction_Ref_Type 
                 := RW_Transaction_Type'Unchecked_Access;
      end record;

   overriding
   procedure Read_Node
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           :    out Nodes.Node_Type);

   procedure Write_Node
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type;
      N_A         : in     Nodes.Valid_Address_Type;
      N           : in     Nodes.Node_Type);


   type Sub_RW_Transaction_Type is new RW_Transaction_Type with
      record
         Owning_Transaction : Transaction_Ref_Type;
      end record;

   overriding
   function New_RW_Transaction
     (Tree : Tree_Type)
      return Sub_RW_Transaction_Type;
   -- Raises a Tree_Error, must never be used. Ada enforces us to override 
   -- New_RW_Transaction from RW_Transaction_Type (an alternative would be 
   -- to source out the constructors so that they are not class members, but
   -- this makes the public part of this specification rather ugly.

   function New_Sub_RW_Transaction
     (Tree               : Tree_Type;
      Owning_Transaction : RO_Transaction_Type'Class)
      return Sub_RW_Transaction_Type;

   overriding
   procedure Start_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out Sub_RW_Transaction_Type);

   overriding
   procedure Abort_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out Sub_RW_Transaction_Type);

   overriding
   procedure Commit_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out Sub_RW_Transaction_Type);


   type Bound_Kind_Type is (Concrete_Bound, Abstract_Bound);
   type Direction_Type is (From_Upper_To_Lower, From_Lower_To_Upper);
   type Infinity_Type is (Negative_Infinity, Positive_Infinity);
   type Bound_Type (Kind : Bound_Kind_Type := Concrete_Bound) is
      record
         case Kind is
            when Concrete_Bound =>
               Comparison : Comparison_Type;
               Key        : Key_Type;
            when Abstract_Bound =>
               Location   : Infinity_Type;
         end case;
      end record;

   type Nullable_Transaction_Ref_Type is access all Transaction_Type'Class;
   pragma Controlled (Nullable_Transaction_Ref_Type);
   for Nullable_Transaction_Ref_Type'Storage_Size use 0;

   type Cursor_Type is
      record
         Final              : Boolean                       := False;
         Lower_Bound        : Bound_Type;
         Upper_Bound        : Bound_Type;
         Direction          : Direction_Type;
         Has_Node           : Boolean                       := False;
         Node               : Nodes.Node_Type;
         Index              : Nodes.Valid_Index_Type;
         Force_Recalibrate  : Boolean                       := False;
         Owning_Tree        : Tree_Ref_Type;
         Initialized        : Boolean                       := False;
         Thread_Safe        : Boolean;
         Mutex              : Locks.Mutexes.Mutex_Type;
         Owning_Transaction : Nullable_Transaction_Ref_Type := null;
      end record;


end DB.Gen_BTrees;
