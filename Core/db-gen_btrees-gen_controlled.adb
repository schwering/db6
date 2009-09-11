package body DB.Gen_BTrees.Gen_Controlled is

   overriding
   procedure Finalize (Tree : in out Tree_Type)
   is begin
      Gen_BTrees.Finalize(Tree.Tree);
   end Finalize;


   overriding
   procedure Finalize (Transaction : in out RO_Transaction_Type)
   is begin
      if Transaction.Transaction.Owning_Tree /= null then
         Gen_BTrees.Finish_Transaction(Transaction.Transaction.Owning_Tree.all,
                                       Transaction.Transaction);
      end if;
   end Finalize;


   overriding
   procedure Finalize (Transaction : in out RW_Transaction_Type)
   is begin
      if Transaction.Transaction.Owning_Tree /= null then
         Gen_BTrees.Abort_Transaction(Transaction.Transaction.Owning_Tree.all,
                                      Transaction.Transaction);
      end if;
   end Finalize;


   overriding
   procedure Finalize (Cursor : in out Cursor_Type)
   is begin
      if Cursor.Cursor.Owning_Tree /= null then
         Gen_BTrees.Finalize(Cursor.Cursor.Owning_Tree.all, Cursor.Cursor);
      end if;
   end Finalize;


   function Transaction_Impl
     (Transaction : Transaction_Type'Class)
      return Transaction_Ref_Type
   is begin
      if Transaction in RO_Transaction_Type'Class then
         return RO_Transaction_Type(Transaction).Transaction.Self;
      elsif Transaction in RW_Transaction_Type'Class then
         return RW_Transaction_Type(Transaction).Transaction.Self;
      else
         raise Tree_Error;
      end if;
   end Transaction_Impl;


   procedure Create
     (ID : in String)
   is begin
      Gen_BTrees.Create(ID);
   end Create;


   procedure Initialize
     (Tree : out Tree_Type'Class;
      ID   : in  String)
   is begin
      Gen_BTrees.Initialize(Tree.Tree, ID);
   end Initialize;


   procedure Finalize (Tree : in out Tree_Type'Class)
   is begin
      Gen_BTrees.Finalize(Tree.Tree);
   end Finalize;


   function Max_Key_Size
     (Max_Value_Size : IO.Blocks.Size_Type
                     := IO.Blocks.Bits_To_Units(Value_Type'Size))
      return IO.Blocks.Size_Type
   is begin
      return Gen_BTrees.Max_Key_Size(Max_Value_Size);
   end Max_Key_Size;


   function New_RO_Transaction
     (Tree : Tree_Type'Class)
      return RO_Transaction_Type
   is begin
      return (AF.Limited_Controlled with
              Gen_BTrees.New_RO_Transaction(Tree.Tree));
   end New_RO_Transaction;


   function New_RW_Transaction
     (Tree : Tree_Type'Class)
      return RW_Transaction_Type
   is begin
      return (AF.Limited_Controlled with
              Gen_BTrees.New_RW_Transaction(Tree.Tree));
   end New_RW_Transaction;


   procedure Start_Transaction
     (Tree        : in out Tree_Type'Class;
      Transaction :    out RO_Transaction_Type)
   is begin
      Gen_BTrees.Start_Transaction(Tree.Tree, Transaction.Transaction);
   end Start_Transaction;


   procedure Start_Transaction
     (Tree        : in out Tree_Type'Class;
      Transaction :    out RW_Transaction_Type)
   is begin
      Gen_BTrees.Start_Transaction(Tree.Tree, Transaction.Transaction);
   end Start_Transaction;


   procedure Finish_Transaction
     (Tree        : in out Tree_Type'Class;
      Transaction :    out RO_Transaction_Type)
   is begin
      Gen_BTrees.Finish_Transaction(Tree.Tree,
                                    Transaction.Transaction);
   end Finish_Transaction;


   procedure Abort_Transaction
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type)
   is begin
      Gen_BTrees.Abort_Transaction(Tree.Tree,
                                   Transaction.Transaction);
   end Abort_Transaction;


   procedure Commit_Transaction
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type)
   is begin
      Gen_BTrees.Commit_Transaction(Tree.Tree,
                                    Transaction.Transaction);
   end Commit_Transaction;


   procedure Look_Up
     (Tree     : in out Tree_Type'Class;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is begin
      Gen_BTrees.Look_Up(Tree.Tree, Key, Value,
                         Gen_BTrees.Count_Type(Position),
                         Gen_BTrees.Result_Type(State));
   end Look_Up;


   procedure Look_Up
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is begin
      Gen_BTrees.Look_Up(Tree.Tree, Transaction_Impl(Transaction).all,
                         Key, Value,
                         Gen_BTrees.Count_Type(Position),
                         Gen_BTrees.Result_Type(State));
   end Look_Up;


   procedure Look_Up
     (Tree     : in out Tree_Type'Class;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type)
   is begin
      Gen_BTrees.Look_Up(Tree.Tree, Gen_BTrees.Count_Type(Position),
                         Value, Key, Gen_BTrees.Result_Type(State));
   end Look_Up;


   procedure Look_Up
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type)
   is begin
      Gen_BTrees.Look_Up(Tree.Tree, Transaction_Impl(Transaction).all,
                         Gen_BTrees.Count_Type(Position), Value, Key,
                         Gen_BTrees.Result_Type(State));
   end Look_Up;


   procedure Minimum
     (Tree     : in out Tree_Type'Class;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is begin
      Gen_BTrees.Minimum(Tree.Tree, Key, Value,
                         Gen_BTrees.Count_Type(Position),
                         Gen_BTrees.Result_Type(State));
   end Minimum;


   procedure Minimum
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is begin
      Gen_BTrees.Minimum(Tree.Tree, Transaction_Impl(Transaction).all,
                         Key, Value, Gen_BTrees.Count_Type(Position),
                         Gen_BTrees.Result_Type(State));
   end Minimum;


   procedure Maximum
     (Tree     : in out Tree_Type'Class;
      Key      :    out Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is begin
      Gen_BTrees.Maximum(Tree.Tree, Key, Value,
                         Gen_BTrees.Count_Type(Position),
                         Gen_BTrees.Result_Type(State));
   end Maximum;


   procedure Maximum
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is begin
      Gen_BTrees.Maximum(Tree.Tree, Transaction_Impl(Transaction).all,
                         Key, Value, Gen_BTrees.Count_Type(Position),
                         Gen_BTrees.Result_Type(State));
   end Maximum;


   procedure Insert
     (Tree     : in out Tree_Type'Class;
      Key      : in     Key_Type;
      Value    : in     Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is begin
      Gen_BTrees.Insert(Tree.Tree, Key, Value,
                        Gen_BTrees.Count_Type(Position),
                        Gen_BTrees.Result_Type(State));
   end Insert;

   procedure Insert
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       : in     Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is begin
      Gen_BTrees.Insert(Tree.Tree, Transaction.Transaction,
                        Key, Value, Gen_BTrees.Count_Type(Position),
                        Gen_BTrees.Result_Type(State));
   end Insert;


   procedure Delete
     (Tree     : in out Tree_Type'Class;
      Key      : in     Key_Type;
      Value    :    out Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is begin
      Gen_BTrees.Delete(Tree.Tree, Key, Value,
                        Gen_BTrees.Count_Type(Position),
                        Gen_BTrees.Result_Type(State));
   end Delete;


   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Key_Type;
      Value       :    out Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is begin
      Gen_BTrees.Delete(Tree.Tree, Transaction.Transaction,
                        Key, Value, Gen_BTrees.Count_Type(Position),
                        Gen_BTrees.Result_Type(State));
   end Delete;


   procedure Delete
     (Tree     : in out Tree_Type'Class;
      Position : in     Count_Type;
      Value    :    out Value_Type;
      Key      :    out Key_Type;
      State    :    out Result_Type)
   is begin
      Gen_BTrees.Delete(Tree.Tree, Gen_BTrees.Count_Type(Position),
                        Value, Key, Gen_BTrees.Result_Type(State));
   end Delete;


   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Value_Type;
      Key         :    out Key_Type;
      State       :    out Result_Type)
   is begin
      Gen_BTrees.Delete(Tree.Tree, Transaction.Transaction,
                        Gen_BTrees.Count_Type(Position), Value, Key,
                        Gen_BTrees.Result_Type(State));
   end Delete;


   procedure Count
     (Tree : in out Tree_Type'Class;
      N    :    out Count_Type)
   is begin
      Gen_BTrees.Count(Tree.Tree, Gen_BTrees.Count_Type(N));
   end Count;


   procedure Count
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      N           :    out Count_Type)
   is begin
      Gen_BTrees.Count(Tree.Tree, Transaction_Impl(Transaction).all,
                       Gen_BTrees.Count_Type(N));
   end Count;


   procedure Get_Height
     (Tree   : in out Tree_Type'Class;
      Height :    out Height_Type)
   is begin
      Gen_BTrees.Get_Height(Tree.Tree, Gen_BTrees.Height_Type(Height));
   end Get_Height;


   procedure Get_Height
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Height_Type)
   is begin
      Gen_BTrees.Get_Height(Tree.Tree, Transaction_Impl(Transaction).all,
                            Gen_BTrees.Height_Type(Height));
   end Get_Height;


   procedure Clusterize
     (Tree  : in out Tree_Type'Class;
      State :    out Result_Type)
   is begin
      Gen_BTrees.Clusterize(Tree.Tree, Gen_BTrees.Result_Type(State));
   end Clusterize;


   function Positive_Infinity_Bound
      return Bound_Type
   is begin
      return Bound_Type(Gen_BTrees.Positive_Infinity_Bound);
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound
      return Bound_Type
   is begin
      return Bound_Type(Gen_BTrees.Negative_Infinity_Bound);
   end Negative_Infinity_Bound;


   function New_Bound
     (Comparison : Comparison_Type;
      Key        : Key_Type)
      return Bound_Type
   is begin
      return Bound_Type(Gen_BTrees.New_Bound
                          (Gen_BTrees.Comparison_Type(Comparison), Key));
   end New_Bound;


   function New_Cursor
     (Tree              : Tree_Type'Class;
      Transaction       : Transaction_Type'Class;
      Thread_Safe       : Boolean;
      Lower_Bound       : Bound_Type;
      Upper_Bound       : Bound_Type;
      Reverse_Direction : Boolean := False)
      return Cursor_Type
   is begin
      return (AF.Limited_Controlled with
              Gen_BTrees.New_Cursor(Tree.Tree,
                                    Transaction_Impl(Transaction).all,
                                    Thread_Safe,
                                    Gen_BTrees.Bound_Type(Lower_Bound),
                                    Gen_BTrees.Bound_Type(Upper_Bound),
                                    Reverse_Direction));
   end New_Cursor;


   procedure Finalize
     (Tree   : in out Tree_Type'Class;
      Cursor : in out Cursor_Type'Class)
   is begin
      Gen_BTrees.Finalize(Tree.Tree, Cursor.Cursor);
   end Finalize;


   procedure Pause
     (Tree   : in out Tree_Type'Class;
      Cursor : in out Cursor_Type'Class)
   is begin
      Gen_BTrees.Pause(Tree.Tree, Cursor.Cursor);
   end Pause;


   procedure Unpause
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class)
   is begin
      Gen_BTrees.Unpause(Tree.Tree, Transaction_Impl(Transaction).all,
                         Cursor.Cursor);
   end Unpause;


   procedure Next
     (Tree        : in out Tree_Type'Class;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class;
      Key         :    out Key_Type;
      Value       :    out Value_Type;
      State       :    out Result_Type)
   is begin
      Gen_BTrees.Next(Tree.Tree, Transaction_Impl(Transaction).all,
                      Cursor.Cursor, Key, Value, Gen_BTrees.Result_Type(State));
   end Next;


   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RO_Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class;
      State       :    out Result_Type)
   is begin
      Gen_BTrees.Delete(Tree.Tree, Transaction.Transaction,
                        Cursor.Cursor, Gen_BTrees.Result_Type(State));
   end Delete;


   procedure Delete
     (Tree        : in out Tree_Type'Class;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type'Class;
      State       :    out Result_Type)
   is begin
      Gen_BTrees.Delete(Tree.Tree, Transaction.Transaction,
                        Cursor.Cursor, Gen_BTrees.Result_Type(State));
   end Delete;

end DB.Gen_BTrees.Gen_Controlled;

