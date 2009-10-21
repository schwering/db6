package body DB.Tables.Maps is

   function New_RO_Transaction
     (Map : Map_Type)
      return RO_Transaction_Type
   is begin
      if Map.Short then
         return (Short             => True,
                 Short_Transaction =>
                    BTrees.New_RO_Transaction(Map.Short_Tree));
      else
         return (Short            => False,
                 Long_Transaction =>
                    Blob_Trees.New_RO_Transaction(Map.Long_Tree));
      end if;
   end New_RO_Transaction;


   procedure Start_Transaction
     (Map         : in out Map_Type;
      Transaction : in out RO_Transaction_Type)
   is
      pragma Assert (Map.Short = Transaction.Short);
   begin
      if Map.Short then
         BTrees.Start_Transaction(Map.Short_Tree,
                                  Transaction.Short_Transaction);
      else
         Blob_Trees.Start_Transaction(Map.Long_Tree,
                                      Transaction.Long_Transaction);
      end if;
   end Start_Transaction;


   procedure Finish_Transaction
     (Map         : in out Map_Type;
      Transaction : in out RO_Transaction_Type)
   is
      pragma Assert (Map.Short = Transaction.Short);
   begin
      if Map.Short then
         BTrees.Finish_Transaction(Map.Short_Tree,
                                   Transaction.Short_Transaction);
      else
         Blob_Trees.Finish_Transaction(Map.Long_Tree,
                                       Transaction.Long_Transaction);
      end if;
   end Finish_Transaction;


   function New_RW_Transaction
     (Map : Map_Type)
      return RW_Transaction_Type
   is begin
      if Map.Short then
         return (Short             => True,
                 Short_Transaction =>
                    BTrees.New_RW_Transaction(Map.Short_Tree));
      else
         return (Short            => False,
                 Long_Transaction =>
                    Blob_Trees.New_RW_Transaction(Map.Long_Tree));
      end if;
   end New_RW_Transaction;


   procedure Start_Transaction
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type)
   is
      pragma Assert (Map.Short = Transaction.Short);
   begin
      if Map.Short then
         BTrees.Start_Transaction(Map.Short_Tree,
                                  Transaction.Short_Transaction);
      else
         Blob_Trees.Start_Transaction(Map.Long_Tree,
                                      Transaction.Long_Transaction);
      end if;
   end Start_Transaction;


   procedure Abort_Transaction
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type)
   is
      pragma Assert (Map.Short = Transaction.Short);
   begin
      if Map.Short then
         BTrees.Abort_Transaction(Map.Short_Tree,
                                  Transaction.Short_Transaction);
      else
         Blob_Trees.Abort_Transaction(Map.Long_Tree,
                                      Transaction.Long_Transaction);
      end if;
   end Abort_Transaction;


   procedure Commit_Transaction
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type)
   is
      pragma Assert (Map.Short = Transaction.Short);
   begin
      if Map.Short then
         BTrees.Commit_Transaction(Map.Short_Tree,
                                   Transaction.Short_Transaction);
      else
         Blob_Trees.Commit_Transaction(Map.Long_Tree,
                                       Transaction.Long_Transaction);
      end if;
   end Commit_Transaction;


   function New_Map
     (Max_Key_Size   : in IO.Blocks.Size_Type;
      Max_Value_Size : in IO.Blocks.Size_Type)
      return Map_Type
   is
      use type IO.Blocks.Size_Type;
   begin
      if Max_Key_Size <= BTrees.Max_Key_Size(Max_Value_Size) then
         return Map_Type'(Short => True, others => <>);
      else
         return Map_Type'(Short => False, others => <>);
      end if;
   end New_Map;


   procedure Create
     (ID             : in String;
      Max_Key_Size   : in IO.Blocks.Size_Type;
      Max_Value_Size : in IO.Blocks.Size_Type)
   is
      use type IO.Blocks.Size_Type;
   begin
      if Max_Key_Size <= BTrees.Max_Key_Size(Max_Value_Size) then
         BTrees.Create(ID);
      else
         Blob_Trees.Create(ID);
      end if;
   end Create;


   procedure Initialize
     (Map : out Map_Type;
      ID  : in  String)
   is begin
      if Map.Short then
         BTrees.Initialize(Map.Short_Tree, ID);
      else
         Blob_Trees.Initialize(Map.Long_Tree, ID);
      end if;
   end Initialize;


   procedure Finalize
     (Map : in out Map_Type)
   is begin
      if Map.Short then
         BTrees.Finalize(Map.Short_Tree);
      else
         Blob_Trees.Finalize(Map.Long_Tree);
      end if;
   end Finalize;


   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : IO.Blocks.Size_Type)
      return IO.Blocks.Size_Type
   is begin
      if Map.Short then
         return BTrees.Max_Key_Size(Max_Value_Size);
      else
         return Blob_Trees.Max_Key_Size;
      end if;
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
     (State : Blob_Trees.Result_Type)
      return Result_Type
   is
      use type Blob_Trees.Result_Type;
   begin
      case State is
         when Blob_Trees.Success => return Success;
         when Blob_Trees.Failure => return Failure;
         when Blob_Trees.Error   => return Error;
      end case;
   end To_State;


   procedure Look_Up
     (Map      : in out Map_Type;
      Key      : in     Types.Keys.Key_Type;
      Value    :    out Types.Values.Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Look_Up(Map.Short_Tree, Key, Value,
                           BTrees.Count_Type(Position), S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Look_Up(Map.Long_Tree, Key, Value,
                               Blob_Trees.Count_Type(Position), S);
            State := To_State(S);
         end;
      end if;
   end Look_Up;


   procedure Look_Up
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Key         : in     Types.Keys.Key_Type;
      Value       :    out Types.Values.Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      pragma Assert ((Transaction not in RO_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short) and
                     (Transaction not in RW_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short));
   begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            if Transaction in RO_Transaction_Type'Class then
               BTrees.Look_Up(Map.Short_Tree,
                             RO_Transaction_Type(Transaction).Short_Transaction,
                             Key, Value, BTrees.Count_Type(Position), S);
            else
               BTrees.Look_Up(Map.Short_Tree,
                             RW_Transaction_Type(Transaction).Short_Transaction,
                             Key, Value, BTrees.Count_Type(Position), S);
            end if;
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            if Transaction in RO_Transaction_Type'Class then
               Blob_Trees.Look_Up(Map.Long_Tree,
                             RO_Transaction_Type(Transaction).Long_Transaction,
                             Key, Value, Blob_Trees.Count_Type(Position), S);
            else
               Blob_Trees.Look_Up(Map.Long_Tree,
                             RW_Transaction_Type(Transaction).Long_Transaction,
                             Key, Value, Blob_Trees.Count_Type(Position), S);
            end if;
            State := To_State(S);
         end;
      end if;
   end Look_Up;


   procedure Look_Up
     (Map      : in out Map_Type;
      Position : in     Count_Type;
      Value    :    out Types.Values.Value_Type;
      Key      :    out Types.Keys.Key_Type;
      State    :    out Result_Type)
   is begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Look_Up(Map.Short_Tree, BTrees.Count_Type(Position), Value,
                           Key, S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Look_Up(Map.Long_Tree, Blob_Trees.Count_Type(Position),
                               Value, Key, S);
            State := To_State(S);
         end;
      end if;
   end Look_Up;


   procedure Look_Up
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Types.Values.Value_Type;
      Key         :    out Types.Keys.Key_Type;
      State       :    out Result_Type)
   is
      pragma Assert ((Transaction not in RO_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short) and
                     (Transaction not in RW_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short));
   begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            if Transaction in RO_Transaction_Type'Class then
               BTrees.Look_Up(Map.Short_Tree,
                             RO_Transaction_Type(Transaction).Short_Transaction,
                             BTrees.Count_Type(Position), Value, Key, S);
            else
               BTrees.Look_Up(Map.Short_Tree,
                             RW_Transaction_Type(Transaction).Short_Transaction,
                             BTrees.Count_Type(Position), Value, Key, S);
            end if;
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            if Transaction in RO_Transaction_Type'Class then
               Blob_Trees.Look_Up(Map.Long_Tree,
                             RO_Transaction_Type(Transaction).Long_Transaction,
                             Blob_Trees.Count_Type(Position), Value, Key, S);
            else
               Blob_Trees.Look_Up(Map.Long_Tree,
                             RW_Transaction_Type(Transaction).Long_Transaction,
                             Blob_Trees.Count_Type(Position), Value, Key, S);
            end if;
            State := To_State(S);
         end;
      end if;
   end Look_Up;


   procedure Minimum
     (Map      : in out Map_Type;
      Key      :    out Types.Keys.Key_Type;
      Value    :    out Types.Values.Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Minimum(Map.Short_Tree, Key, Value,
                           BTrees.Count_Type(Position), S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Minimum(Map.Long_Tree, Key, Value,
                               Blob_Trees.Count_Type(Position), S);
            State := To_State(S);
         end;
      end if;
   end Minimum;


   procedure Minimum
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Types.Keys.Key_Type;
      Value       :    out Types.Values.Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      pragma Assert ((Transaction not in RO_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short) and
                     (Transaction not in RW_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short));
   begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            if Transaction in RO_Transaction_Type'Class then
               BTrees.Minimum(Map.Short_Tree,
                             RO_Transaction_Type(Transaction).Short_Transaction,
                             Key, Value, BTrees.Count_Type(Position), S);
            else
               BTrees.Minimum(Map.Short_Tree,
                             RW_Transaction_Type(Transaction).Short_Transaction,
                             Key, Value, BTrees.Count_Type(Position), S);
            end if;
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            if Transaction in RO_Transaction_Type'Class then
               Blob_Trees.Minimum(Map.Long_Tree,
                             RO_Transaction_Type(Transaction).Long_Transaction,
                             Key, Value, Blob_Trees.Count_Type(Position), S);
            else
               Blob_Trees.Minimum(Map.Long_Tree,
                             RW_Transaction_Type(Transaction).Long_Transaction,
                             Key, Value, Blob_Trees.Count_Type(Position), S);
            end if;
            State := To_State(S);
         end;
      end if;
   end Minimum;


   procedure Maximum
     (Map      : in out Map_Type;
      Key      :    out Types.Keys.Key_Type;
      Value    :    out Types.Values.Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Maximum(Map.Short_Tree, Key, Value,
                           BTrees.Count_Type(Position), S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Maximum(Map.Long_Tree, Key, Value,
                               Blob_Trees.Count_Type(Position), S);
            State := To_State(S);
         end;
      end if;
   end Maximum;


   procedure Maximum
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Key         :    out Types.Keys.Key_Type;
      Value       :    out Types.Values.Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      pragma Assert ((Transaction not in RO_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short) and
                     (Transaction not in RW_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short));
   begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            if Transaction in RO_Transaction_Type'Class then
               BTrees.Maximum(Map.Short_Tree,
                             RO_Transaction_Type(Transaction).Short_Transaction,
                             Key, Value, BTrees.Count_Type(Position), S);
            else
               BTrees.Maximum(Map.Short_Tree,
                             RW_Transaction_Type(Transaction).Short_Transaction,
                             Key, Value, BTrees.Count_Type(Position), S);
            end if;
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            if Transaction in RO_Transaction_Type'Class then
               Blob_Trees.Maximum(Map.Long_Tree,
                             RO_Transaction_Type(Transaction).Long_Transaction,
                             Key, Value, Blob_Trees.Count_Type(Position), S);
            else
               Blob_Trees.Maximum(Map.Long_Tree,
                             RW_Transaction_Type(Transaction).Long_Transaction,
                             Key, Value, Blob_Trees.Count_Type(Position), S);
            end if;
            State := To_State(S);
         end;
      end if;
   end Maximum;


   procedure Insert
     (Map      : in out Map_Type;
      Key      : in     Types.Keys.Key_Type;
      Value    : in     Types.Values.Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Insert(Map.Short_Tree, Key, Value,
                          BTrees.Count_Type(Position), S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Insert(Map.Long_Tree, Key, Value,
                              Blob_Trees.Count_Type(Position), S);
            State := To_State(S);
         end;
      end if;
   end Insert;


   procedure Insert
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Types.Keys.Key_Type;
      Value       : in     Types.Values.Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Map.Short = Transaction.Short);
   begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Insert(Map.Short_Tree, Transaction.Short_Transaction,
                          Key, Value, BTrees.Count_Type(Position), S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Insert(Map.Long_Tree, Transaction.Long_Transaction,
                              Key, Value, Blob_Trees.Count_Type(Position), S);
            State := To_State(S);
         end;
      end if;
   end Insert;


   procedure Delete
     (Map      : in out Map_Type;
      Key      : in     Types.Keys.Key_Type;
      Value    :    out Types.Values.Value_Type;
      Position :    out Count_Type;
      State    :    out Result_Type)
   is begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Delete(Map.Short_Tree, Key, Value,
                          BTrees.Count_Type(Position), S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Delete(Map.Long_Tree, Key, Value,
                              Blob_Trees.Count_Type(Position), S);
            State := To_State(S);
         end;
      end if;
   end Delete;


   procedure Delete
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Key         : in     Types.Keys.Key_Type;
      Value       :    out Types.Values.Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Map.Short = Transaction.Short);
   begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Delete(Map.Short_Tree, Transaction.Short_Transaction,
                          Key, Value, BTrees.Count_Type(Position), S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Delete(Map.Long_Tree, Transaction.Long_Transaction,
                              Key, Value, Blob_Trees.Count_Type(Position), S);
            State := To_State(S);
         end;
      end if;
   end Delete;


   procedure Delete
     (Map      : in out Map_Type;
      Position : in     Count_Type;
      Value    :    out Types.Values.Value_Type;
      Key      :    out Types.Keys.Key_Type;
      State    :    out Result_Type)
   is begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Delete(Map.Short_Tree, BTrees.Count_Type(Position),
                          Value, Key, S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Delete(Map.Long_Tree, Blob_Trees.Count_Type(Position),
                              Value, Key, S);
            State := To_State(S);
         end;
      end if;
   end Delete;


   procedure Delete
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Position    : in     Count_Type;
      Value       :    out Types.Values.Value_Type;
      Key         :    out Types.Keys.Key_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Map.Short = Transaction.Short);
   begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Delete(Map.Short_Tree, Transaction.Short_Transaction,
                          BTrees.Count_Type(Position), Value, Key, S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Delete(Map.Long_Tree, Transaction.Long_Transaction,
                              Blob_Trees.Count_Type(Position), Value, Key, S);
            State := To_State(S);
         end;
      end if;
   end Delete;


   function Positive_Infinity_Bound
     (Map : Map_Type)
      return Bound_Type
   is begin
      if Map.Short then
         return Bound_Type'(Short       => True,
                            Short_Bound => BTrees.Positive_Infinity_Bound);
      else
         return Bound_Type'(Short      => False,
                            Long_Bound => Blob_Trees.Positive_Infinity_Bound);
      end if;
   end Positive_Infinity_Bound;


   function Negative_Infinity_Bound
     (Map : Map_Type)
      return Bound_Type
   is begin
      if Map.Short then
         return Bound_Type'(Short       => True,
                            Short_Bound => BTrees.Negative_Infinity_Bound);
      else
         return Bound_Type'(Short      => False,
                            Long_Bound => Blob_Trees.Negative_Infinity_Bound);
      end if;
   end Negative_Infinity_Bound;


   function New_Bound
     (Map        : Map_Type;
      Comparison : Comparison_Type;
      Key        : Types.Keys.Key_Type)
      return Bound_Type
   is begin
      if Map.Short then
         declare
            C : BTrees.Comparison_Type;
         begin
            case Comparison is
               when Less             => C := BTrees.Less;
               when Less_Or_Equal    => C := BTrees.Less_Or_Equal;
               when Equal            => C := BTrees.Equal;
               when Greater_Or_Equal => C := BTrees.Greater_Or_Equal;
               when Greater          => C := BTrees.Greater;
            end case;
            return Bound_Type'(Short       => True,
                               Short_Bound => BTrees.New_Bound(C, Key));
         end;
      else
         declare
            C : Blob_Trees.Comparison_Type;
         begin
            case Comparison is
               when Less             => C := Blob_Trees.Less;
               when Less_Or_Equal    => C := Blob_Trees.Less_Or_Equal;
               when Equal            => C := Blob_Trees.Equal;
               when Greater_Or_Equal => C := Blob_Trees.Greater_Or_Equal;
               when Greater          => C := Blob_Trees.Greater;
            end case;
            return Bound_Type'(Short      => False,
                               Long_Bound => Blob_Trees.New_Bound(C, Key));
         end;
      end if;
   end New_Bound;


   function New_Cursor
     (Map               : Map_Type;
      Transaction       : Transaction_Type'Class;
      Thread_Safe       : Boolean;
      Lower_Bound       : Bound_Type;
      Upper_Bound       : Bound_Type;
      Reverse_Direction : Boolean := False)
      return Cursor_Type
   is
      pragma Assert ((Transaction not in RO_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short) and
                     (Transaction not in RW_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short));
      pragma Assert (Map.Short = Lower_Bound.Short);
      pragma Assert (Map.Short = Upper_Bound.Short);
   begin
      if Map.Short then
         if Transaction in RO_Transaction_Type'Class then
            return (Short        => True,
                    Short_Cursor =>
                       BTrees.New_Cursor
                         (Map.Short_Tree,
                          RO_Transaction_Type(Transaction).Short_Transaction,
                          Thread_Safe,
                          Lower_Bound.Short_Bound,
                          Upper_Bound.Short_Bound,
                          Reverse_Direction));
         else
            return (Short        => True,
                    Short_Cursor =>
                       BTrees.New_Cursor
                         (Map.Short_Tree,
                          RW_Transaction_Type(Transaction).Short_Transaction,
                          Thread_Safe,
                          Lower_Bound.Short_Bound,
                          Upper_Bound.Short_Bound,
                          Reverse_Direction));
         end if;
      else
         if Transaction in RO_Transaction_Type'Class then
            return (Short       => False,
                    Long_Cursor =>
                       Blob_Trees.New_Cursor
                         (Map.Long_Tree,
                          RO_Transaction_Type(Transaction).Long_Transaction,
                          Thread_Safe,
                          Lower_Bound.Long_Bound,
                          Upper_Bound.Long_Bound,
                          Reverse_Direction));
         else
            return (Short       => False,
                    Long_Cursor =>
                       Blob_Trees.New_Cursor
                         (Map.Long_Tree,
                          RW_Transaction_Type(Transaction).Long_Transaction,
                          Thread_Safe,
                          Lower_Bound.Long_Bound,
                          Upper_Bound.Long_Bound,
                          Reverse_Direction));
         end if;
      end if;
   end New_Cursor;


   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean)
   is begin
      if Cursor.Short then
         BTrees.Set_Thread_Safety(Cursor.Short_Cursor, Enabled);
      else
         Blob_Trees.Set_Thread_Safety(Cursor.Long_Cursor, Enabled);
      end if;
   end Set_Thread_Safety;


   procedure Finalize
     (Map    : in out Map_Type;
      Cursor : in out Cursor_Type)
   is
      pragma Assert (Map.Short = Cursor.Short);
   begin
      if Cursor.Short then
         BTrees.Finalize(Map.Short_Tree, Cursor.Short_Cursor);
      else
         Blob_Trees.Finalize(Map.Long_Tree, Cursor.Long_Cursor);
      end if;
   end Finalize;


   procedure Pause
     (Map    : in out Map_Type;
      Cursor : in out Cursor_Type)
   is
      pragma Assert (Map.Short = Cursor.Short);
   begin
      if Cursor.Short then
         BTrees.Pause(Map.Short_Tree, Cursor.Short_Cursor);
      else
         Blob_Trees.Pause(Map.Long_Tree, Cursor.Long_Cursor);
      end if;
   end Pause;


   procedure Unpause
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type)
   is
      pragma Assert (Map.Short = Cursor.Short);
      pragma Assert ((Transaction not in RO_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short) and
                     (Transaction not in RW_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short));
   begin
      if Map.Short then
         if Transaction in RO_Transaction_Type'Class then
            BTrees.Unpause(Map.Short_Tree,
                           RO_Transaction_Type(Transaction).Short_Transaction,
                           Cursor.Short_Cursor);
         else
            BTrees.Unpause(Map.Short_Tree,
                           RO_Transaction_Type(Transaction).Short_Transaction,
                           Cursor.Short_Cursor);
         end if;
      else
         if Transaction in RO_Transaction_Type'Class then
            Blob_Trees.Unpause(Map.Long_Tree,
                           RO_Transaction_Type(Transaction).Long_Transaction,
                           Cursor.Long_Cursor);
         else
            Blob_Trees.Unpause(Map.Long_Tree,
                           RO_Transaction_Type(Transaction).Long_Transaction,
                           Cursor.Long_Cursor);
         end if;
      end if;
   end Unpause;


   procedure Next
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Types.Keys.Key_Type;
      Value       :    out Types.Values.Value_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Map.Short = Cursor.Short);
      pragma Assert ((Transaction not in RO_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short) and
                     (Transaction not in RW_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short));
   begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            if Transaction in RO_Transaction_Type'Class then
               BTrees.Next(Map.Short_Tree,
                           RO_Transaction_Type(Transaction).Short_Transaction,
                           Cursor.Short_Cursor, Key, Value, S);
            else
               BTrees.Next(Map.Short_Tree,
                           RW_Transaction_Type(Transaction).Short_Transaction,
                           Cursor.Short_Cursor, Key, Value, S);
            end if;
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            if Transaction in RO_Transaction_Type'Class then
               Blob_Trees.Next(Map.Long_Tree,
                              RO_Transaction_Type(Transaction).Long_Transaction,
                              Cursor.Long_Cursor, Key, Value, S);
            else
               Blob_Trees.Next(Map.Long_Tree,
                              RW_Transaction_Type(Transaction).Long_Transaction,
                              Cursor.Long_Cursor, Key, Value, S);
            end if;
            State := To_State(S);
         end;
      end if;
   end Next;


   procedure Delete
     (Map         : in out Map_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Cursor      : in out Cursor_Type;
      Key         :    out Types.Keys.Key_Type;
      Value       :    out Types.Values.Value_Type;
      Position    :    out Count_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Map.Short = Cursor.Short);
      pragma Assert (Map.Short = Transaction.Short);
   begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Delete(Map.Short_Tree, Transaction.Short_Transaction,
                          Cursor.Short_Cursor, Key, Value,
                          BTrees.Count_Type(Position), S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Delete(Map.Long_Tree, Transaction.Long_Transaction,
                              Cursor.Long_Cursor, Key, Value,
                              Blob_Trees.Count_Type(Position), S);
            State := To_State(S);
         end;
      end if;
   end Delete;


   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type)
   is begin
      if Map.Short then
         BTrees.Count(Map.Short_Tree, BTrees.Count_Type(Count));
      else
         Blob_Trees.Count(Map.Long_Tree, Blob_Trees.Count_Type(Count));
      end if;
   end Count;


   procedure Count
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Count       :    out Count_Type)
   is
      pragma Assert ((Transaction not in RO_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short) and
                     (Transaction not in RW_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short));
   begin
      if Map.Short then
         if Transaction in RO_Transaction_Type'Class then
            BTrees.Count(Map.Short_Tree,
                         RO_Transaction_Type(Transaction).Short_Transaction,
                         BTrees.Count_Type(Count));
         else
            BTrees.Count(Map.Short_Tree,
                         RW_Transaction_Type(Transaction).Short_Transaction,
                         BTrees.Count_Type(Count));
         end if;
      else
         if Transaction in RO_Transaction_Type'Class then
            Blob_Trees.Count(Map.Long_Tree,
                             RO_Transaction_Type(Transaction).Long_Transaction,
                             Blob_Trees.Count_Type(Count));
         else
            Blob_Trees.Count(Map.Long_Tree,
                             RW_Transaction_Type(Transaction).Long_Transaction,
                             Blob_Trees.Count_Type(Count));
         end if;
      end if;
   end Count;


   procedure Get_Height
     (Map    : in out Map_Type;
      Height :    out Height_Type)
   is begin
      if Map.Short then
         BTrees.Get_Height(Map.Short_Tree, BTrees.Height_Type(Height));
      else
         Blob_Trees.Get_Height(Map.Long_Tree, Blob_Trees.Height_Type(Height));
      end if;
   end Get_Height;


   procedure Get_Height
     (Map         : in out Map_Type;
      Transaction : in out Transaction_Type'Class;
      Height      :    out Height_Type)
   is
      pragma Assert ((Transaction not in RO_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short) and
                     (Transaction not in RW_Transaction_Type'Class or else
                      Map.Short = RO_Transaction_Type(Transaction).Short));
   begin
      if Map.Short then
         if Transaction in RO_Transaction_Type'Class then
            BTrees.Get_Height(Map.Short_Tree,
                             RO_Transaction_Type(Transaction).Short_Transaction,
                             BTrees.Height_Type(Height));
         else
            BTrees.Get_Height(Map.Short_Tree,
                             RW_Transaction_Type(Transaction).Short_Transaction,
                             BTrees.Height_Type(Height));
         end if;
      else
         if Transaction in RO_Transaction_Type'Class then
            Blob_Trees.Get_Height(Map.Long_Tree,
                              RO_Transaction_Type(Transaction).Long_Transaction,
                              Blob_Trees.Height_Type(Height));
         else
            Blob_Trees.Get_Height(Map.Long_Tree,
                              RW_Transaction_Type(Transaction).Long_Transaction,
                              Blob_Trees.Height_Type(Height));
         end if;
      end if;
   end Get_Height;


   procedure Clusterize
     (Map   : in out Map_Type;
      State :    out Result_Type)
   is begin
      if Map.Short then
         declare
            S : BTrees.Result_Type;
         begin
            BTrees.Clusterize(Map.Short_Tree, S);
            State := To_State(S);
         end;
      else
         declare
            S : Blob_Trees.Result_Type;
         begin
            Blob_Trees.Clusterize(Map.Long_Tree, S);
            State := To_State(S);
         end;
      end if;
   end Clusterize;

end DB.Tables.Maps;

