with Ada.Unchecked_Conversion;

package body DB.Gen_Blob_Trees is

   package body BTree_Utils is
      procedure Write_Value
        (Context : in out Value_Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Value   : in     Value_Type)
      is
         pragma Unreferenced (Context);
         procedure Write is new IO.Blocks.Write(Value_Type);
      begin
         Write(Block, Cursor, Value);
      end Write_Value;


      procedure Read_Value
        (Context : in out Value_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Value   :    out Value_Type)
      is
         pragma Unreferenced (Context);
         procedure Read is new IO.Blocks.Read(Value_Type);
      begin
         Read(Block, Cursor, Value);
      end Read_Value;


      procedure Skip_Value
        (Context : in out Value_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type)
      is
         Value : Value_Type;
      begin
         Read_Value(Context, Block, Cursor, Value);
      end Skip_Value;
   end BTree_Utils;


   package body Heap_Utils is
      function Info_Index_ID
        (ID : String)
         return String
      is begin
         return ID &".info.ix";
      end Info_Index_ID;


      function Free_Index_ID
        (ID : String)
         return String
      is begin
         return ID &".free.ix";
      end Free_Index_ID;
   end Heap_Utils;


   function New_RO_Transaction
     (Tree : Tree_Type)
      return RO_Transaction_Type
   is begin
      return (BTree_Transaction => BTrees.New_RO_Transaction(Tree.BTree),
              Heap_Transaction  => Heaps.New_RO_Transaction(Tree.Heap));
   end New_RO_Transaction;


   procedure Start_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type)
   is begin
      BTrees.Start_Transaction(Tree.BTree, Transaction.BTree_Transaction);
      Heaps.Start_Transaction(Tree.Heap, Transaction.Heap_Transaction);
   end Start_Transaction;


   procedure Finish_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RO_Transaction_Type)
   is begin
      Heaps.Finish_Transaction(Tree.Heap, Transaction.Heap_Transaction);
      BTrees.Finish_Transaction(Tree.BTree, Transaction.BTree_Transaction);
   end Finish_Transaction;


   function New_RW_Transaction
     (Tree : Tree_Type)
      return RW_Transaction_Type
   is begin
      return (BTree_Transaction => BTrees.New_RW_Transaction(Tree.BTree),
              Heap_Transaction  => Heaps.New_RW_Transaction(Tree.Heap));
   end New_RW_Transaction;


   procedure Start_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type)
   is begin
      BTrees.Start_Transaction(Tree.BTree, Transaction.BTree_Transaction);
      Heaps.Start_Transaction(Tree.Heap, Transaction.Heap_Transaction);
   end Start_Transaction;


   procedure Abort_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type)
   is begin
      Heaps.Abort_Transaction(Tree.Heap, Transaction.Heap_Transaction);
      BTrees.Abort_Transaction(Tree.BTree, Transaction.BTree_Transaction);
   end Abort_Transaction;


   procedure Commit_Transaction
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type)
   is begin
      Heaps.Commit_Transaction(Tree.Heap, Transaction.Heap_Transaction);
      BTrees.Commit_Transaction(Tree.BTree, Transaction.BTree_Transaction);
   end Commit_Transaction;


   BTree_Suffix : constant String := ".btree";
   Heap_Suffix  : constant String := ".heap";


   procedure Create
     (ID : in String)
   is begin
      BTrees.Create(ID & BTree_Suffix);
      Heaps.Create(ID & Heap_Suffix);
   end Create;


   procedure Initialize
     (Tree : out Tree_Type;
      ID   : in  String)
   is begin
      BTrees.Initialize(Tree.BTree, ID & BTree_Suffix);
      Heaps.Initialize(Tree.Heap, ID & Heap_Suffix);
   end Initialize;


   procedure Finalize
     (Tree : in out Tree_Type)
   is begin
      BTrees.Finalize(Tree.BTree);
      Heaps.Finalize(Tree.Heap);
   end Finalize;


   function Max_Key_Size
      return IO.Blocks.Size_Type
   is begin
      return BTrees.Max_Key_Size(IO.Blocks.Bits_To_Units
                                       (BTree_Utils.Value_Type'Size));
   end Max_Key_Size;


   function To_State
     (State : BTrees.Result_Type)
      return Result_Type
   is
      use type BTrees.Result_Type;
   begin
      case State is
         when BTrees.Success => return Success;
         when BTrees.Failure => return Failure;
         when BTrees.Error   => return Error;
      end case;
   end To_State;


   function To_State
     (State : Heaps.Result_Type)
      return Result_Type
   is
      use type Heaps.Result_Type;
   begin
      case State is
         when Heaps.Success => return Success;
         when Heaps.Failure => return Failure;
         when Heaps.Error   => return Error;
      end case;
   end To_State;


   procedure Look_Up
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      BTrees.Look_Up(Tree.BTree, Key, Address, BTrees.Count_Type(Position),
                     B_State);
      State := To_State(B_State);
      if State = Success then
         Heaps.Get(Tree.Heap, Address, Value, H_State);
         State := To_State(H_State);
      end if;
   end Look_Up;


   procedure Look_Up
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      if Transaction in RO_Transaction_Type'Class then
         BTrees.Look_Up(Tree.BTree,
                        RO_Transaction_Type(Transaction).BTree_Transaction,
                        Key, Address, BTrees.Count_Type(Position), B_State);
      else
         BTrees.Look_Up(Tree.BTree,
                        RW_Transaction_Type(Transaction).BTree_Transaction,
                        Key, Address, BTrees.Count_Type(Position), B_State);
      end if;
      State := To_State(B_State);
      if State = Success then
         if Transaction in RO_Transaction_Type'Class then
            Heaps.Get(Tree.Heap,
                      RO_Transaction_Type(Transaction).Heap_Transaction,
                      Address, Value, H_State);
         else
            Heaps.Get(Tree.Heap,
                      RW_Transaction_Type(Transaction).Heap_Transaction,
                      Address, Value, H_State);
         end if;
         State := To_State(H_State);
      end if;
   end Look_Up;


   procedure Look_Up
     (Tree     : in out Tree_Type;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      BTrees.Look_Up(Tree.BTree, BTrees.Count_Type(Position), Address, Key,
                     B_State);
      State := To_State(B_State);
      if State = Success then
         Heaps.Get(Tree.Heap, Address, Value, H_State);
         State := To_State(H_State);
      end if;
   end Look_Up;


   procedure Look_Up
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      if Transaction in RO_Transaction_Type'Class then
         BTrees.Look_Up(Tree.BTree,
                        RO_Transaction_Type(Transaction).BTree_Transaction,
                        BTrees.Count_Type(Position), Address, Key, B_State);
      else
         BTrees.Look_Up(Tree.BTree,
                        RW_Transaction_Type(Transaction).BTree_Transaction,
                        BTrees.Count_Type(Position), Address, Key, B_State);
      end if;
      State := To_State(B_State);
      if State = Success then
         if Transaction in RO_Transaction_Type'Class then
            Heaps.Get(Tree.Heap,
                      RO_Transaction_Type(Transaction).Heap_Transaction,
                      Address, Value, H_State);
         else
            Heaps.Get(Tree.Heap,
                      RW_Transaction_Type(Transaction).Heap_Transaction,
                      Address, Value, H_State);
         end if;
         State := To_State(H_State);
      end if;
   end Look_Up;


   procedure Minimum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      BTrees.Minimum(Tree.BTree, Key, Address, BTrees.Count_Type(Position),
                     B_State);
      State := To_State(B_State);
      if State = Success then
         Heaps.Get(Tree.Heap, Address, Value, H_State);
         State := To_State(H_State);
      end if;
   end Minimum;


   procedure Minimum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      if Transaction in RO_Transaction_Type'Class then
         BTrees.Minimum(Tree.BTree,
                        RO_Transaction_Type(Transaction).BTree_Transaction,
                        Key, Address, BTrees.Count_Type(Position), B_State);
      else
         BTrees.Minimum(Tree.BTree,
                        RW_Transaction_Type(Transaction).BTree_Transaction,
                        Key, Address, BTrees.Count_Type(Position), B_State);
      end if;
      State := To_State(B_State);
      if State = Success then
         if Transaction in RO_Transaction_Type'Class then
            Heaps.Get(Tree.Heap,
                      RO_Transaction_Type(Transaction).Heap_Transaction,
                      Address, Value, H_State);
         else
            Heaps.Get(Tree.Heap,
                      RW_Transaction_Type(Transaction).Heap_Transaction,
                      Address, Value, H_State);
         end if;
         State := To_State(H_State);
      end if;
   end Minimum;


   procedure Maximum
     (Tree     : in out Tree_Type;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      BTrees.Maximum(Tree.BTree, Key, Address, BTrees.Count_Type(Position),
                     B_State);
      State := To_State(B_State);
      if State = Success then
         Heaps.Get(Tree.Heap, Address, Value, H_State);
         State := To_State(H_State);
      end if;
   end Maximum;


   procedure Maximum
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      if Transaction in RO_Transaction_Type'Class then
         BTrees.Maximum(Tree.BTree,
                        RO_Transaction_Type(Transaction).BTree_Transaction,
                        Key, Address, BTrees.Count_Type(Position), B_State);
      else
         BTrees.Maximum(Tree.BTree,
                        RW_Transaction_Type(Transaction).BTree_Transaction,
                        Key, Address, BTrees.Count_Type(Position), B_State);
      end if;
      State := To_State(B_State);
      if State = Success then
         if Transaction in RO_Transaction_Type'Class then
            Heaps.Get(Tree.Heap,
                      RO_Transaction_Type(Transaction).Heap_Transaction,
                      Address, Value, H_State);
         else
            Heaps.Get(Tree.Heap,
                      RW_Transaction_Type(Transaction).Heap_Transaction,
                      Address, Value, H_State);
         end if;
         State := To_State(H_State);
      end if;
   end Maximum;


   procedure Insert
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      Heaps.Put(Tree.Heap, Value, Address, H_State);
      State := To_State(H_State);
      if State = Success then
         BTrees.Insert(Tree.BTree, Key, Address, BTrees.Count_Type(Position),
                       B_State);
         State := To_State(B_State);
      end if;
   end Insert;


   procedure Insert
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      Heaps.Put(Tree.Heap, Transaction.Heap_Transaction, Value, Address,
                H_State);
      State := To_State(H_State);
      if State = Success then
         BTrees.Insert(Tree.BTree, Transaction.BTree_Transaction, Key, Address,
                       BTrees.Count_Type(Position), B_State);
         State := To_State(B_State);
      end if;
   end Insert;


   procedure Delete
     (Tree     : in out Tree_Type;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      BTrees.Delete(Tree.BTree, Key, Address, BTrees.Count_Type(Position),
                    B_State);
      State := To_State(B_State);
      if State = Success then
         Heaps.Get(Tree.Heap, Address, Value, H_State);
         State := To_State(H_State);
         if State = Success then
            Heaps.Delete(Tree.Heap, Address, H_State);
            State := To_State(H_State);
         end if;
      end if;
   end Delete;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      BTrees.Delete(Tree.BTree, Transaction.BTree_Transaction, Key, Address,
                    BTrees.Count_Type(Position), B_State);
      State := To_State(B_State);
      if State = Success then
         Heaps.Get(Tree.Heap, Transaction.Heap_Transaction, Address, Value,
                   H_State);
         State := To_State(H_State);
         if State = Success then
            Heaps.Delete(Tree.Heap, Transaction.Heap_Transaction, Address,
                         H_State);
            State := To_State(H_State);
         end if;
      end if;
   end Delete;


   procedure Delete
     (Tree     : in out Tree_Type;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      BTrees.Delete(Tree.BTree, BTrees.Count_Type(Position), Address, Key,
                    B_State);
      State := To_State(B_State);
      if State = Success then
         Heaps.Get(Tree.Heap, Address, Value, H_State);
         State := To_State(H_State);
         if State = Success then
            Heaps.Delete(Tree.Heap, Address, H_State);
            State := To_State(H_State);
         end if;
      end if;
   end Delete;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      BTrees.Delete(Tree.BTree, Transaction.BTree_Transaction,
                    BTrees.Count_Type(Position), Address, Key, B_State);
      State := To_State(B_State);
      if State = Success then
         Heaps.Get(Tree.Heap, Transaction.Heap_Transaction, Address, Value,
                   H_State);
         State := To_State(H_State);
         if State = Success then
            Heaps.Delete(Tree.Heap, Transaction.Heap_Transaction, Address,
                         H_State);
            State := To_State(H_State);
         end if;
      end if;
   end Delete;


   function Positive_Infinity_Bound
      return Bound_Type
   is
      -- We use Unchecked_Conversion because a normal cast leads to a
      -- compilation error of the generic instances for some reason (has
      -- something to do with the hidden constraints of BTrees.Bound_Type).
      function Convert is new Ada.Unchecked_Conversion
         (BTrees.Bound_Type, Bound_Type);
      Bound : constant BTrees.Bound_Type := BTrees.Positive_Infinity_Bound;
   begin
      return Convert(Bound);
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound
      return Bound_Type
   is
      -- We use Unchecked_Conversion because a normal cast leads to a
      -- compilation error of the generic instances for some reason (has
      -- something to do with the hidden constraints of BTrees.Bound_Type).
      function Convert is new Ada.Unchecked_Conversion
         (BTrees.Bound_Type, Bound_Type);
      Bound : constant BTrees.Bound_Type := BTrees.Negative_Infinity_Bound;
   begin
      return Convert(Bound);
   end Negative_Infinity_Bound;


   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type
   is
      C     : BTrees.Comparison_Type;
   begin
      case Comparison is
         when Less             => C := BTrees.Less;
         when Less_Or_Equal    => C := BTrees.Less_Or_Equal;
         when Equal            => C := BTrees.Equal;
         when Greater_Or_Equal => C := BTrees.Greater_Or_Equal;
         when Greater          => C := BTrees.Greater;
      end case;
      declare
         -- We use Unchecked_Conversion because a normal cast leads to a
         -- compilation error of the generic instances for some reason (has
         -- something to do with the hidden constraints of BTrees.Bound_Type).
         function Convert is new Ada.Unchecked_Conversion
            (BTrees.Bound_Type, Bound_Type);
         Bound : constant BTrees.Bound_Type := BTrees.New_Bound(C, Key);
      begin
         return Convert(Bound);
      end;
   end New_Bound;


   function New_Cursor
     (Tree              : Tree_Type;
      Transaction       : Transaction_Type'Class;
      Thread_Safe       : Boolean;
      Lower_Bound       : Bound_Type;
      Upper_Bound       : Bound_Type;
      Reverse_Direction : Boolean := False)
      return Cursor_Type
   is begin
      if Transaction in RO_Transaction_Type'Class then
         return (Cursor => BTrees.New_Cursor(Tree.BTree,
                             RO_Transaction_Type(Transaction).BTree_Transaction,
                             Thread_Safe,
                             BTrees.Bound_Type(Lower_Bound),
                             BTrees.Bound_Type(Upper_Bound),
                             Reverse_Direction));
      else
         return (Cursor => BTrees.New_Cursor(Tree.BTree,
                             RW_Transaction_Type(Transaction).BTree_Transaction,
                             Thread_Safe,
                             BTrees.Bound_Type(Lower_Bound),
                             BTrees.Bound_Type(Upper_Bound),
                             Reverse_Direction));
      end if;
   end New_Cursor;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean)
   is begin
      BTrees.Set_Thread_Safety(Cursor.Cursor, Enabled);
   end Set_Thread_Safety;


   procedure Finalize
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type)
   is begin
      BTrees.Finalize(Tree.BTree, Cursor.Cursor);
   end Finalize;


   procedure Pause
     (Tree   : in out Tree_Type;
      Cursor : in out Cursor_Type)
   is begin
      BTrees.Pause(Tree.BTree, Cursor.Cursor);
   end Pause;


   procedure Unpause
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type)
   is begin
      if Transaction in RO_Transaction_Type'Class then
         BTrees.Unpause(Tree.BTree,
                        RO_Transaction_Type(Transaction).BTree_Transaction,
                        Cursor.Cursor);
      else
         BTrees.Unpause(Tree.BTree,
                        RO_Transaction_Type(Transaction).BTree_Transaction,
                        Cursor.Cursor);
      end if;
   end Unpause;


   procedure Next
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      if Transaction in RO_Transaction_Type'Class then
         BTrees.Next(Tree.BTree,
                     RO_Transaction_Type(Transaction).BTree_Transaction,
                     Cursor.Cursor, Key, Address, B_State);
      else
         BTrees.Next(Tree.BTree,
                     RW_Transaction_Type(Transaction).BTree_Transaction,
                     Cursor.Cursor, Key, Address, B_State);
      end if;
      State := To_State(B_State);
      if State = Success then
         if Transaction in RO_Transaction_Type'Class then
            Heaps.Get(Tree.Heap,
                      RO_Transaction_Type(Transaction).Heap_Transaction,
                      Address, Value, H_State);
         else
            Heaps.Get(Tree.Heap,
                      RW_Transaction_Type(Transaction).Heap_Transaction,
                      Address, Value, H_State);
         end if;
         State := To_State(H_State);
      end if;
   end Next;


   procedure Delete
     (Tree        : in out Tree_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      Address : BTree_Utils.Value_Type;
      B_State : BTrees.Result_Type;
      H_State : Heaps.Result_Type;
   begin
      BTrees.Delete(Tree.BTree, Transaction.BTree_Transaction, Cursor.Cursor,
                    Key, Address, BTrees.Count_Type(Position), B_State);
      State := To_State(B_State);
      if State = Success then
         Heaps.Get(Tree.Heap, Transaction.Heap_Transaction, Address, Value,
                   H_State);
         State := To_State(H_State);
         if State = Success then
            Heaps.Delete(Tree.Heap, Transaction.Heap_Transaction, Address,
                         H_State);
            State := To_State(H_State);
         end if;
      end if;
   end Delete;


   procedure Count
     (Tree  : in out Tree_Type;
      Count :    out Count_Type)
   is begin
      BTrees.Count(Tree.BTree, BTrees.Count_Type(Count));
   end Count;


   procedure Count
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Count       :    out Count_Type)
   is begin
      if Transaction in RO_Transaction_Type'Class then
         BTrees.Count(Tree.BTree,
                      RO_Transaction_Type(Transaction).BTree_Transaction,
                      BTrees.Count_Type(Count));
      else
         BTrees.Count(Tree.BTree,
                      RW_Transaction_Type(Transaction).BTree_Transaction,
                      BTrees.Count_Type(Count));
      end if;
   end Count;


   procedure Get_Height
     (Tree   : in out Tree_Type;
      Height :    out Height_Type)
   is begin
      BTrees.Get_Height(Tree.BTree, BTrees.Height_Type(Height));
   end Get_Height;


   procedure Get_Height
     (Tree        : in out Tree_Type;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Height_Type)
   is begin
      if Transaction in RO_Transaction_Type'Class then
         BTrees.Get_Height(Tree.BTree,
                           RO_Transaction_Type(Transaction).BTree_Transaction,
                           BTrees.Height_Type(Height));
      else
         BTrees.Get_Height(Tree.BTree,
                           RW_Transaction_Type(Transaction).BTree_Transaction,
                           BTrees.Height_Type(Height));
      end if;
   end Get_Height;


   procedure Clusterize
     (Tree  : in out Tree_Type;
      State :    out Result_Type)
   is
      B_State : BTrees.Result_Type;
   begin
      BTrees.Clusterize(Tree.BTree, B_State);
      State := To_State(B_State);
   end Clusterize;

end DB.Gen_Blob_Trees;

