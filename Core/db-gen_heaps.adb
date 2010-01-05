-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils.Gen_Minimum;
with DB.Utils.Print;

package body DB.Gen_Heaps is

   ----------
   -- Info and Free BTree type packages.

   package body Info_BTree_Types is

      function Key_Size_Bound
        (Key : Key_Type)
         return IO.Blocks.Size_Type
      is
         function Size_Of is new IO.Blocks.Size_Of(Key_Type);
      begin
         return Size_Of(Key);
      end Key_Size_Bound;


      procedure Read_Key
        (Context : in out Key_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Key     :    out Key_Type)
      is
         pragma Unreferenced (Context);
         procedure Read is new IO.Blocks.Read(Key_Type);
      begin
         Read(Block, Cursor, Key);
      end Read_Key;


      procedure Skip_Key
        (Context : in out Key_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type)
      is
         Key : Key_Type;
      begin
         Read_Key(Context, Block, Cursor, Key);
      end Skip_Key;


      procedure Write_Key
        (Context : in out Key_Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Key     : in     Key_Type)
      is
         pragma Unreferenced (Context);
         procedure Write is new IO.Blocks.Write(Key_Type);
      begin
         Write(Block, Cursor, Key);
      end Write_Key;


      function Value_Size_Bound
        (Value : Value_Type)
         return IO.Blocks.Size_Type
      is
         function Size_Of is new IO.Blocks.Size_Of(Chunk_State_Type);
         function Size_Of is new IO.Blocks.Size_Of(Length_Type);
         function Size_Of is new IO.Blocks.Size_Of(Address_Type);
         function Size_Of is new IO.Blocks.Size_Of(Extended_Address_Type);
         use type IO.Blocks.Size_Type;
         Size : IO.Blocks.Size_Type := 0;
      begin
         Size := Size + Size_Of(Value.State);
         Size := Size + Size_Of(Value.Length);
         case Value.State is
            when Used_First =>
               Size := Size + Size_Of(Value.Succ);
               Size := Size + Size_Of(Value.Last);
               Size := Size + Context_Size_Bound(Value.Context);
            when Used_Cont =>
               Size := Size + Size_Of(Value.Succ);
            when Free =>
               null;
         end case;
         return Size;
      end Value_Size_Bound;


      procedure Read_Value
        (Context : in out Value_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Value   :    out Value_Type)
      is
         pragma Unreferenced (Context);
         procedure Read is new IO.Blocks.Read(Chunk_State_Type);
         procedure Read is new IO.Blocks.Read(Length_Type);
         procedure Read_A is new IO.Blocks.Read(Address_Type);
         procedure Read_EA is new IO.Blocks.Read(Extended_Address_Type);
      begin
         Read(Block, Cursor, Value.State);
         Read(Block, Cursor, Value.Length);
         case Value.State is
            when Used_First =>
               Read_EA(Block, Cursor, Value.Succ);
               Read_A(Block, Cursor, Value.Last);
               Read_Context(Block, Cursor, Value.Context);
            when Used_Cont =>
               Read_EA(Block, Cursor, Value.Succ);
            when Free =>
               null;
         end case;
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


      procedure Write_Value
        (Context : in out Value_Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Value   : in     Value_Type)
      is
         pragma Unreferenced (Context);
         procedure Write is new IO.Blocks.Write(Chunk_State_Type);
         procedure Write is new IO.Blocks.Write(Length_Type);
         procedure Write_A is new IO.Blocks.Write(Address_Type);
         procedure Write_EA is new IO.Blocks.Write(Extended_Address_Type);
      begin
         Write(Block, Cursor, Value.State);
         Write(Block, Cursor, Value.Length);
         case Value.State is
            when Used_First =>
               Write_EA(Block, Cursor, Value.Succ);
               Write_A(Block, Cursor, Value.Last);
               Write_Context(Block, Cursor, Value.Context);
            when Used_Cont =>
               Write_EA(Block, Cursor, Value.Succ);
            when Free =>
               null;
         end case;
      end Write_Value;


      function "=" (A, B : Key_Type) return Boolean is
      begin
         return Block_IO."="(A, B);
      end "=";


      function "<=" (A, B : Key_Type) return Boolean is
      begin
         pragma Warnings (Off); -- the reversed order is intended
         return not Block_IO."<"(B, A);
         pragma Warnings (On);
      end "<=";

   end Info_BTree_Types;


   package body Free_BTree_Types is

      function Key_Size_Bound
        (Key : Key_Type)
         return IO.Blocks.Size_Type
      is
         function Size_Of is new IO.Blocks.Size_Of(Key_Type);
      begin
         return Size_Of(Key);
      end Key_Size_Bound;


      procedure Read_Key
        (Context : in out Key_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Key     :    out Key_Type)
      is
         pragma Unreferenced (Context);
         procedure Read is new IO.Blocks.Read(Key_Type);
      begin
         Read(Block, Cursor, Key);
      end Read_Key;


      procedure Skip_Key
        (Context : in out Key_Context_Type;
         Block   : in     IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type)
      is
         Key : Key_Type;
      begin
         Read_Key(Context, Block, Cursor, Key);
      end Skip_Key;


      procedure Write_Key
        (Context : in out Key_Context_Type;
         Block   : in out IO.Blocks.Base_Block_Type;
         Cursor  : in out IO.Blocks.Cursor_Type;
         Key     : in     Key_Type)
      is
         pragma Unreferenced (Context);
         procedure Write is new IO.Blocks.Write(Key_Type);
      begin
         Write(Block, Cursor, Key);
      end Write_Key;


      function Value_Size_Bound
        (Value : Value_Type)
         return IO.Blocks.Size_Type
      is
         function Size_Of is new IO.Blocks.Size_Of(Value_Type);
      begin
         return Size_Of(Value);
      end Value_Size_Bound;


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


      function "=" (A, B : Key_Type) return Boolean
      is
         use type IO.Blocks.Size_Type;
         use Block_IO;
      begin
         return A.Length = B.Length and then A.Address = B.Address;
      end "=";


      function "<=" (A, B : Key_Type) return Boolean
      is
         use type IO.Blocks.Size_Type;
         use Block_IO;
      begin
         return A.Length < B.Length or else
               (A.Length = B.Length and then not (B.Address < A.Address));
      end "<=";

   end Free_BTree_Types;


   ----------
   -- Some small helpers.

   function Block_Identity
     (Block : IO.Blocks.Block_Type)
      return IO.Blocks.Block_Type is
   begin
      return Block;
   end Block_Identity;


   function Chunk_Length
     (Size : Length_Type)
      return Length_Type
   is
      Block_Count  : constant Length_Type
                   := Size / Length_Type(IO.Blocks.Block_Size);
      Chunk_Length : constant Length_Type
                   := Block_Count * Length_Type(IO.Blocks.Block_Size);
   begin
      if Chunk_Length < Size then
         return Chunk_Length + Length_Type(IO.Blocks.Block_Size);
      else
         return Chunk_Length;
      end if;
   end Chunk_Length;


   function Chunk_Length
     (Size : IO.Blocks.Size_Type)
      return Length_Type is
   begin
      return Chunk_Length(Length_Type(Size));
   end Chunk_Length;


   function "+" (Chunk_Length : Length_Type;
                 Item_Size    : IO.Blocks.Size_Type)
                 return Length_Type is
     use type IO.Blocks.Size_Type;
   begin
      return Chunk_Length + Length_Type(Item_Size);
   end "+";


   function Succ
     (Address : Address_Type;
      Count   : Length_Type)
      return Address_Type
   is
      Succ_Address : Address_Type := Address;
   begin
      for I in 1 .. Count loop
         Succ_Address := Block_IO.Succ(Succ_Address);
      end loop;
      return Succ_Address;
   end Succ;


   function Result
     (State : Info_BTrees.Result_Type)
      return Result_Type is
   begin
      case State is
         when Info_BTrees.Success =>
            return Success;
         when Info_BTrees.Failure =>
            return Failure;
         when Info_BTrees.Error =>
            return Error;
      end case;
   end Result;


   function Result
     (State : Free_BTrees.Result_Type)
      return Result_Type is
   begin
      case State is
         when Free_BTrees.Success =>
            return Success;
         when Free_BTrees.Failure =>
            return Failure;
         when Free_BTrees.Error =>
            return Error;
      end case;
   end Result;


   ----------
   -- The first meat! Creation etc.

   procedure Create
     (ID : in String)
   is
      File : Block_IO.File_Type;
   begin
      Block_IO.Create(ID, File);
      Block_IO.Close(File);
      Info_BTrees.Create(Info_Index_ID(ID));
      Free_BTrees.Create(Free_Index_ID(ID));
   end Create;


   procedure Initialize
     (Heap : out Heap_Type;
      ID   : in  String)
   is
      pragma Assert (not Heap.Initialized);
      pragma Assert (not Heap.Finalized);
   begin
      Block_IO.Open(ID, Heap.File);
      Info_BTrees.Initialize(Heap.Info_Tree, Info_Index_ID(ID));
      Free_BTrees.Initialize(Heap.Free_Tree, Free_Index_ID(ID));
      if Block_IO.Needs_Explicit_Block_Count then
         declare
            use type IO.Blocks.Size_Type;
            use type Info_BTrees.Result_Type;
            Address : Address_Type;
            Info    : Chunk_Info_Type;
            Pos     : Info_BTrees.Count_Type;
            St      : Info_BTrees.Result_Type;
         begin
            Info_BTrees.Maximum(Heap.Info_Tree, Address, Info, Pos, St);
            if St = Info_BTrees.Success then
               Address := Succ(Address, Chunk_Length(Info.Length) /
                                          Length_Type(IO.Blocks.Block_Size));
               Block_IO.Set_Block_Count(Heap.File,
                                        Block_IO.To_Address(Address));
            else
               Block_IO.Set_Block_Count(Heap.File, Block_IO.Invalid_Address);
            end if;
         end;
      end if;
      Heap.Initialized := True;
   end Initialize;


   procedure Finalize
     (Heap : in out Heap_Type) is
   begin
      if Heap.Initialized then
         Block_IO.Close(Heap.File);
         Info_BTrees.Finalize(Heap.Info_Tree);
         Free_BTrees.Finalize(Heap.Free_Tree);
         Heap.Initialized := False;
      end if;
      Heap.Finalized := True;
   end Finalize;


   ----------
   -- Transaction procedures.

   function New_RO_Transaction
     (Heap : Heap_Type)
      return RO_Transaction_Type
   is
      pragma Assert (Heap.Initialized);
   begin
      return (Owning_Heap      => Heap.Self,
              Initialized      => True,
              Info_Transaction =>
                  Info_BTrees.New_RO_Transaction(Heap.Info_Tree),
              others           => <>);
   end New_RO_Transaction;


   procedure Start_Transaction
     (Heap        : in out Heap_Type;
      Transaction : in out RO_Transaction_Type)
   is
      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      pragma Assert (Transaction.Initialized);
      pragma Assert (not Transaction.Started);
   begin
      Transaction.Started := True;
      Block_IO.Acquire_Ticket(Heap.File, Transaction.Ticket);
      Block_IO.Read_Lock(Heap.File, Transaction.Ticket);
      Info_BTrees.Start_Transaction(Heap.Info_Tree,
                                    Transaction.Info_Transaction);
   end Start_Transaction;


   procedure Finish_Transaction
     (Heap        : in out Heap_Type;
      Transaction : in out RO_Transaction_Type)
   is
      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Started);
   begin
      Info_BTrees.Finish_Transaction(Heap.Info_Tree,
                                     Transaction.Info_Transaction);
      Block_IO.Unlock(Heap.File, Transaction.Ticket);
      Block_IO.Release_Ticket(Heap.File, Transaction.Ticket);
      Transaction.Started := False;
   end Finish_Transaction;


   function New_RW_Transaction
     (Heap : Heap_Type)
      return RW_Transaction_Type
   is
      pragma Assert (Heap.Initialized);
   begin
      return (Owning_Heap      => Heap.Self,
              Initialized      => True,
              Info_Transaction =>
                  Info_BTrees.New_RW_Transaction(Heap.Info_Tree),
              Free_Transaction =>
                  Free_BTrees.New_RW_Transaction(Heap.Free_Tree),
              others           => <>);
   end New_RW_Transaction;


   procedure Start_Transaction
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type)
   is
      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      pragma Assert (Transaction.Initialized);
      pragma Assert (not Transaction.Started);
   begin
      Transaction.Buffer  := IO_Buffers.New_Buffer;
      Transaction.Started := True;
      Block_IO.Acquire_Ticket(Heap.File, Transaction.Ticket);
      Block_IO.Write_Lock(Heap.File, Transaction.Ticket);
      Info_BTrees.Start_Transaction(Heap.Info_Tree,
                                    Transaction.Info_Transaction);
      Free_BTrees.Start_Transaction(Heap.Free_Tree,
                                    Transaction.Free_Transaction);
   end Start_Transaction;


   procedure Abort_Transaction
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type)
   is
      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Started);
   begin
      Info_BTrees.Abort_Transaction(Heap.Info_Tree,
                                    Transaction.Info_Transaction);
      Free_BTrees.Abort_Transaction(Heap.Free_Tree,
                                    Transaction.Free_Transaction);
      Block_IO.Unlock(Heap.File, Transaction.Ticket);
      Block_IO.Release_Ticket(Heap.File, Transaction.Ticket);
      IO_Buffers.Free(Transaction.Buffer);
      Transaction.Started := False;
   end Abort_Transaction;


   procedure Commit_Transaction
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type)
   is
      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      pragma Assert (Transaction.Initialized);
      pragma Assert (Transaction.Started);
   begin
      Block_IO.Certify_Lock(Heap.File, Transaction.Ticket);
      Info_BTrees.Commit_Transaction(Heap.Info_Tree,
                                     Transaction.Info_Transaction);
      Free_BTrees.Commit_Transaction(Heap.Free_Tree,
                                     Transaction.Free_Transaction);
      IO_Buffers.Commit(Heap.File, Transaction.Buffer);
      Block_IO.Unlock(Heap.File, Transaction.Ticket);
      Block_IO.Release_Ticket(Heap.File, Transaction.Ticket);
      IO_Buffers.Free(Transaction.Buffer);
      Transaction.Started := False;
   end Commit_Transaction;


   ----------
   -- Wrappers for Get, Put, Append and Delete without Transactions.

   procedure Get
     (Heap    : in out Heap_Type;
      Address : in     Address_Type;
      Item    :    out Item_Type;
      State   :    out Result_Type)
   is
      Transaction : RO_Transaction_Type := New_RO_Transaction(Heap);
   begin
      Start_Transaction(Heap, Transaction);
      Get(Heap, Transaction, Address, Item, State);
      Finish_Transaction(Heap, Transaction);
   exception
      when others =>
         Finish_Transaction(Heap, Transaction);
         raise;
   end Get;


   procedure Put
     (Heap    : in out Heap_Type;
      Item    : in     Item_Type;
      Address :    out Address_Type;
      State   :    out Result_Type)
   is
      Transaction : RW_Transaction_Type := New_RW_Transaction(Heap);
   begin
      Start_Transaction(Heap, Transaction);
      Put(Heap, Transaction, Item, Address, State);
      if State = Success then
         Commit_Transaction(Heap, Transaction);
      else
         Abort_Transaction(Heap, Transaction);
      end if;
   exception
      when others =>
         Abort_Transaction(Heap, Transaction);
         raise;
   end Put;


   procedure Append
     (Heap        : in out Heap_Type;
      Item        : in     Item_Type;
      Address     : in     Address_Type;
      State       :    out Result_Type)
   is
      Transaction : RW_Transaction_Type := New_RW_Transaction(Heap);
   begin
      Start_Transaction(Heap, Transaction);
      Append(Heap, Transaction, Item, Address, State);
      if State = Success then
         Commit_Transaction(Heap, Transaction);
      else
         Abort_Transaction(Heap, Transaction);
      end if;
   exception
      when others =>
         Abort_Transaction(Heap, Transaction);
         raise;
   end Append;

 
   procedure Delete
     (Heap    : in out Heap_Type;
      Address : in     Address_Type;
      State   :    out Result_Type)
   is
      pragma Assert (Heap.Initialized);
      Transaction : RW_Transaction_Type := New_RW_Transaction(Heap);
   begin
      Start_Transaction(Heap, Transaction);
      Delete(Heap, Transaction, Address, State);
      if State = Success then
         Commit_Transaction(Heap, Transaction);
      else
         Abort_Transaction(Heap, Transaction);
      end if;
   exception
      when others =>
         Abort_Transaction(Heap, Transaction);
         raise;
   end Delete;


   ----------
   -- The real meat: the implementations of Get, Put, Append, Delete

   procedure Get
     (Heap        : in out Heap_Type;
      Transaction : in out Transaction_Type'Class;
      Address     : in     Address_Type;
      Item        :    out Item_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);

      procedure Read_Info
        (Heap        : in out Heap_Type;
         Transaction : in out Transaction_Type'Class;
         Address     : in     Address_Type;
         Info        :    out Chunk_Info_Type;
         State       :    out Result_Type)
      is
         use type Info_BTrees.Result_Type;
         Pos  : Info_BTrees.Count_Type;
         St   : Info_BTrees.Result_Type;
      begin
         if Transaction in RW_Transaction_Type'Class then
            Info_BTrees.Look_Up(Heap.Info_Tree,
                              RW_Transaction_Type(Transaction).Info_Transaction,
                              Address, Info, Pos, St);
         else
            Info_BTrees.Look_Up(Heap.Info_Tree,
                              RO_Transaction_Type(Transaction).Info_Transaction,
                              Address, Info, Pos, St);
         end if;
         if St = Info_BTrees.Success and then Info.State = Free then
            State := Failure;
         else
            State := Result(St);
         end if;
      exception
         when others =>
            State := Error;
            pragma Warnings (Off);
            raise;
            pragma Warnings (On);
      end Read_Info;

      procedure Read_Item_From_Chunk
        (Heap          : in out Heap_Type;
         Transaction   : in out Transaction_Type'Class;
         First_Address : in     Address_Type;
         Chunk_Length  : in     Length_Type;
         Context       : in out Item_Context_Type;
         Item          : in out Item_Type;
         Done          :    out Boolean)
      is
         use type IO.Blocks.Size_Type;
         use type IO.Blocks.Base_Position_Type;
         Chunk_Size : constant IO.Blocks.Size_Type
                    := IO.Blocks.Size_Type(Chunk_Length);
         Read_Size  : IO.Blocks.Size_Type := 0;
         Address    : Address_Type        := First_Address;
      begin
         Read_Size := 0;
         loop
            declare
               Block     : IO.Blocks.Block_Type;
               First_Pos : constant IO.Blocks.Base_Position_Type := Block'First;
               Cursor    : IO.Blocks.Cursor_Type
                         := IO.Blocks.New_Cursor(First_Pos);
            begin
               if Transaction in RW_Transaction_Type'Class then
                  IO_Buffers.Read(Heap.File,
                                  RW_Transaction_Type(Transaction).Buffer,
                                  Address, Block);
               else
                  Block_IO.Read(Heap.File, Address, Block);
               end if;
               loop
                  Read_Part_Of_Item(Context, Block, Cursor, Item, Done);
                  exit when not IO.Blocks.Is_Valid(Cursor);
                  Read_Size := Read_Size +
                               IO.Blocks.Moved_Since(Block, Cursor,
                                                     First_Pos);
                  exit when Done;
               end loop;
               Address := Block_IO.Succ(Address);
               exit when Read_Size = Chunk_Size or Done;
            end;
         end loop;
      end Read_Item_From_Chunk;

      Current_Address : Address_Type := Address;
      Context         : Item_Context_Type;
   begin
      loop
         declare
            Info : Chunk_Info_Type;
            Done : Boolean;
         begin
            Read_Info(Heap, Transaction, Current_Address, Info, State);
            if State /= Success then
               return;
            end if;
            pragma Assert (Info.State = Used_First or Info.State = Used_Cont);
            if Info.State = Used_First then
               Context := Info.Context;
            end if;
            Read_Item_From_Chunk(Heap, Transaction, Current_Address,
                                 Info.Length, Context, Item, Done);
            exit when Done;
            pragma Assert (Block_IO.Is_Valid_Address(Info.Succ));
            Current_Address := Block_IO.To_Valid_Address(Info.Succ);
         end;
      end loop;
   exception
      when others =>
         State := Error;
         pragma Warnings (Off);
         raise;
         pragma Warnings (On);
   end Get;


   procedure Set_Free
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Address     : in     Address_Type;
      Length      : in     Length_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Length = Chunk_Length(Length));
   begin
      declare
         use type Free_BTrees.Result_Type;
         Key : constant Free_BTree_Types.Key_Type := (Length, Address);
         Val : Free_BTree_Types.Value_Type;
         Pos : Free_BTrees.Count_Type;
         St  : Free_BTrees.Result_Type;
      begin
         Free_BTrees.Insert(Heap.Free_Tree, Transaction.Free_Transaction,
                            Key, Val, Pos, St);
         if St /= Free_BTrees.Success then
            State := Result(St);
            return;
         end if;
      end;

      declare
         use type Info_BTrees.Result_Type;
         Info : Chunk_Info_Type;
         Pos  : Info_BTrees.Count_Type;
         St   : Info_BTrees.Result_Type;
      begin
         Info.State := Free;
         Info.Length := Length;
         -- rest of Info is not relevant
         Info_BTrees.Insert(Heap.Info_Tree, Transaction.Info_Transaction,
                            Address, Info, Pos, St);
         if St /= Info_BTrees.Success then
            State := Error;
            return;
         end if;
      end;
      State := Success;
   exception
      when others =>
         State := Error;
         pragma Warnings (Off);
         raise;
         pragma Warnings (On);
   end Set_Free;


   procedure Unset_Free
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Address     : in     Address_Type;
      Length      : in     Length_Type;
      State       :    out Result_Type)
   is
      use type IO.Blocks.Size_Type;
      pragma Assert (Length = Chunk_Length(Length));
   begin
      declare
         use type Info_BTrees.Result_Type;
         Info : Chunk_Info_Type;
         Pos  : Info_BTrees.Count_Type;
         St   : Info_BTrees.Result_Type;
      begin
         Info_BTrees.Delete(Heap.Info_Tree, Transaction.Info_Transaction,
                            Address, Info, Pos, St);
         if St /= Info_BTrees.Success then
            State := Result(St);
            return;
         end if;
      end;

      declare
         use type Free_BTrees.Result_Type;
         Key : constant Free_BTree_Types.Key_Type := (Length, Address);
         Val : Free_BTree_Types.Value_Type;
         Pos : Free_BTrees.Count_Type;
         St  : Free_BTrees.Result_Type;
      begin
         Free_BTrees.Delete(Heap.Free_Tree, Transaction.Free_Transaction,
                            Key, Val, Pos, St);
         if St /= Free_BTrees.Success then
            State := Error;
            return;
         end if;
      end;
      State := Success;
   exception
      when others =>
         State := Error;
         pragma Warnings (Off);
         raise;
         pragma Warnings (On);
   end Unset_Free;


   procedure Unset_Free_Following
     (Heap              : in out Heap_Type;
      Transaction       : in out RW_Transaction_Type'Class;
      Address           : in out Address_Type;
      Reverse_Direction : in     Boolean;
      Length            : in out Length_Type;
      State             :    out Result_Type)
   is

      function Lower_Bound
        (Address           : Address_Type;
         Reverse_Direction : Boolean)
         return Info_BTrees.Bound_Type is
      begin
         if not Reverse_Direction then
            return Info_BTrees.New_Bound(Info_BTrees.Greater, Address);
         else
            return Info_BTrees.Negative_Infinity_Bound;
         end if;
      end Lower_Bound;

      function Upper_Bound
        (Address           : Address_Type;
         Reverse_Direction : Boolean)
         return Info_BTrees.Bound_Type is
      begin
         if not Reverse_Direction then
            return Info_BTrees.Positive_Infinity_Bound;
         else
            return Info_BTrees.New_Bound(Info_BTrees.Less, Address);
         end if;
      end Upper_Bound;

      use type IO.Blocks.Size_Type;
      use type Info_BTrees.Result_Type;
      LB     : constant Info_BTrees.Bound_Type
             := Lower_Bound(Address, Reverse_Direction);
      UB     : constant Info_BTrees.Bound_Type
             := Upper_Bound(Address, Reverse_Direction);
      Cursor : Info_BTrees.Cursor_Type
             := Info_BTrees.New_Cursor
                  (Tree              => Heap.Info_Tree,
                   Transaction       => Transaction.Info_Transaction,
                   Thread_Safe       => False,
                   Lower_Bound       => LB,
                   Upper_Bound       => UB,
                   Reverse_Direction => Reverse_Direction);
   begin
      loop
         declare
            Addr : Address_Type;
            Info : Chunk_Info_Type;
            St   : Info_BTrees.Result_Type;
         begin
            Info_BTrees.Next(Heap.Info_Tree, Transaction.Info_Transaction,
                             Cursor, Addr, Info, St);
            if St /= Info_BTrees.Success or else Info.State /= Free then
               State := Success;
               Info_BTrees.Finalize(Heap.Info_Tree, Cursor);
               return;
            end if;
            if Reverse_Direction then
               Address := Addr;
            end if;
            Length := Length + Info.Length;

            declare
               Key : Address_Type;
               Val : Chunk_Info_Type;
               Pos : Info_BTrees.Count_Type;
            begin
               Info_BTrees.Delete(Heap.Info_Tree, Transaction.Info_Transaction,
                                  Cursor, Key, Val, Pos, St);
               if St /= Info_BTrees.Success then
                  State := Error;
                  Info_BTrees.Finalize(Heap.Info_Tree, Cursor);
                  return;
               end if;
            end;

            declare
               use type Free_BTrees.Result_Type;
               Key : constant Free_BTree_Types.Key_Type := (Info.Length, Addr);
               Val : Free_BTree_Types.Value_Type;
               Pos : Free_BTrees.Count_Type;
               St  : Free_BTrees.Result_Type;
            begin
               Free_BTrees.Delete(Heap.Free_Tree, Transaction.Free_Transaction,
                                  Key, Val, Pos, St);
               if St /= Free_BTrees.Success then
                  State := Error;
                  Info_BTrees.Finalize(Heap.Info_Tree, Cursor);
                  return;
               end if;
            end;
         end;
      end loop;
   exception
      when others =>
         State := Error;
         Info_BTrees.Finalize(Heap.Info_Tree, Cursor);
         raise;
   end Unset_Free_Following;


   procedure Put_Helper
     (Heap            : in out Heap_Type;
      Transaction     : in out RW_Transaction_Type'Class;
      Context         : in out Item_Context_Type;
      Item            : in     Item_Type;
      Item_Size_Bound : in     IO.Blocks.Size_Type;
      Chunk_State     : in     Chunk_State_Type;
      Address         :    out Address_Type;
      State           :    out Result_Type)
   is

      procedure Allocate_Chunk
        (Heap            : in out Heap_Type;
         Transaction     : in out RW_Transaction_Type'Class;
         Item_Size_Bound : in     IO.Blocks.Size_Type;
         Address         :    out Address_Type;
         State           :    out Result_Type)
      is

         procedure Recycle_Free_Chunk
           (Heap        : in out Heap_Type;
            Transaction : in out RW_Transaction_Type'Class;
            Min_Length  : in     Length_Type;
            Address     :    out Address_Type;
            Length      :    out Length_Type;
            State       :    out Result_Type)
         is
            use type Free_BTrees.Result_Type;
            LB     : constant Free_BTrees.Bound_Type
                   := Free_BTrees.New_Bound(Free_BTrees.Greater_Or_Equal,
                                            (Min_Length, Block_IO.First));
            UB     : constant Free_BTrees.Bound_Type
                   := Free_BTrees.Positive_Infinity_Bound;
            Cursor : Free_BTrees.Cursor_Type
                   := Free_BTrees.New_Cursor
                       (Tree              => Heap.Free_Tree,
                        Transaction       => Transaction.Free_Transaction,
                        Thread_Safe       => False,
                        Lower_Bound       => LB,
                        Upper_Bound       => UB,
                        Reverse_Direction => False);
            Key    : Free_BTree_Types.Key_Type;
            Val    : Free_BTree_Types.Value_Type;
            St     : Free_BTrees.Result_Type;
         begin
            Free_BTrees.Next(Heap.Free_Tree, Transaction.Free_Transaction,
                             Cursor, Key, Val, St);
            Free_BTrees.Finalize(Heap.Free_Tree, Cursor);
            if St /= Free_BTrees.Success then
               State  := Result(St);
               Length := 0;
               return;
            end if;
            Address := Key.Address;
            Length  := Key.Length;
            Unset_Free(Heap, Transaction, Address, Length, State);
            if State /= Success then
               return;
            end if;
         exception
            when others =>
               State := Error;
               Free_BTrees.Finalize(Heap.Free_Tree, Cursor);
               raise;
         end Recycle_Free_Chunk;

         procedure Merge_Clipping_With_Next_Free_Chunk
           (Heap            : in out Heap_Type;
            Transaction     : in out RW_Transaction_Type'Class;
            Address         : in     Address_Type;
            Chunk_Length    : in     Length_Type;
            Item_Size_Bound : in     IO.Blocks.Size_Type;
            State           :    out Result_Type)
         is
            use type IO.Blocks.Size_Type;
            Needed_Chunk_Length : constant Length_Type
                                := Gen_Heaps.Chunk_Length(Item_Size_Bound);
            Block_Count         : constant Length_Type
                                := Needed_Chunk_Length /
                                   Length_Type(IO.Blocks.Block_Size);
            Free_Chunk_Length   : Length_Type
                                := Chunk_Length - Needed_Chunk_Length;
            Free_Address        : Address_Type
                                := Succ(Address, Block_Count);
         begin
            Unset_Free_Following(Heap              => Heap,
                                 Transaction       => Transaction,
                                 Address           => Free_Address,
                                 Reverse_Direction => False,
                                 Length            => Free_Chunk_Length,
                                 State             => State);
            if State /= Success then
               State := Error;
               return;
            end if;
            if Free_Chunk_Length > 0 then
               Set_Free(Heap, Transaction, Free_Address, Free_Chunk_Length,
                        State);
               if State /= Success then
                  State := Error;
                  return;
               end if;
            end if;
         end Merge_Clipping_With_Next_Free_Chunk;

         Chunk_Length : Length_Type;
      begin
         Recycle_Free_Chunk(Heap        => Heap,
                            Transaction => Transaction,
                            Min_Length  => Length_Type(Item_Size_Bound),
                            Address     => Address,
                            Length      => Chunk_Length,
                            State       => State);
         case State is
            when Success =>
               Merge_Clipping_With_Next_Free_Chunk
                 (Heap            => Heap,
                  Transaction     => Transaction,
                  Address         => Address,
                  Chunk_Length    => Chunk_Length,
                  Item_Size_Bound => Item_Size_Bound,
                  State           => State);
            when Failure =>
               IO_Buffers.Seek_New(Heap.File, Transaction.Buffer, Address);
               State := Success;
            when Error =>
               null;
         end case;
      end Allocate_Chunk;

      procedure Set_Used
        (Heap        : in out Heap_Type;
         Transaction : in out RW_Transaction_Type'Class;
         Address     : in     Address_Type;
         Chunk_State : in     Chunk_State_Type;
         Item_Size   : in     IO.Blocks.Size_Type;
         Context     : in     Item_Context_Type;
         State       :    out Result_Type)
      is
         use type Info_BTrees.Result_Type;
         Info : constant Chunk_Info_Type
              := (State   => Chunk_State,
                  Length  => Length_Type(Item_Size),
                  Succ    => Block_IO.Invalid_Address,
                  Last    => Address,
                  Context => Context);
         Pos  : Info_BTrees.Count_Type;
         St   : Info_BTrees.Result_Type;
      begin
         Info_BTrees.Insert(Heap.Info_Tree, Transaction.Info_Transaction,
                            Address, Info, Pos, St);
         if St /= Info_BTrees.Success then
            State := Result(St);
            return;
         end if;
         State := Success;
      exception
         when others =>
            State := Error;
            pragma Warnings (Off);
            raise;
            pragma Warnings (On);
      end Set_Used;

      procedure Write_Item_To_Chunk
        (Heap          : in out Heap_Type;
         Transaction   : in out RW_Transaction_Type'Class;
         First_Address : in     Address_Type;
         Context       : in out Item_Context_Type;
         Item          : in     Item_Type;
         Written_Size  :    out IO.Blocks.Size_Type)
      is
         Address : Address_Type := First_Address;
      begin
         Written_Size := 0;
         loop
            declare
               use type IO.Blocks.Size_Type;
               use type IO.Blocks.Base_Position_Type;
               Block     : IO.Blocks.Block_Type;
               First_Pos : constant IO.Blocks.Base_Position_Type := Block'First;
               Cursor    : IO.Blocks.Cursor_Type
                         := IO.Blocks.New_Cursor(First_Pos);
               Done      : Boolean;
            begin
               loop
                  Write_Part_Of_Item(Context, Block, Cursor, Item, Done);
                  exit when not IO.Blocks.Is_Valid(Cursor);
                  Written_Size := Written_Size +
                                 IO.Blocks.Moved_Since(Block, Cursor,
                                                       First_Pos);
                  exit when Done;
               end loop;
               IO_Buffers.Write(Heap.File, Transaction.Buffer, Address,
                                IO.Blocks.To_Block(Block, Cursor));
               Address := Block_IO.Succ(Address);
               exit when Done;
            end;
         end loop;
      end Write_Item_To_Chunk;

      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      use type IO.Blocks.Size_Type;
      Written_Size : IO.Blocks.Size_Type;
   begin
      Allocate_Chunk(Heap            => Heap,
                     Transaction     => Transaction,
                     Item_Size_Bound => Item_Size_Bound,
                     Address         => Address,
                     State           => State);
      if State /= Success then
         State := Error;
         return;
      end if;

      Write_Item_To_Chunk(Heap          => Heap,
                          Transaction   => Transaction,
                          First_Address => Address,
                          Context       => Context,
                          Item          => Item,
                          Written_Size  => Written_Size);
      pragma Assert (Written_Size <= Item_Size_Bound);

      Set_Used(Heap, Transaction, Address, Chunk_State, Written_Size, Context,
               State);
      if State /= Success then
         State := Error;
         return;
      end if;

      State := Success;
   exception
      when others =>
         State := Error;
         pragma Warnings (Off);
         raise;
         pragma Warnings (On);
   end Put_Helper;


   procedure Put
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Item        : in     Item_Type;
      Address     :    out Address_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      Item_Size : constant IO.Blocks.Size_Type
                := Gen_Heaps.Item_Size_Bound(Item);
      Context   : Item_Context_Type;
   begin
      Put_Helper(Heap, Transaction, Context, Item, Item_Size, Used_First,
                 Address, State);
   exception
      when others =>
         State := Error;
         pragma Warnings (Off);
         raise;
         pragma Warnings (On);
   end Put;


   procedure Append
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Item        : in     Item_Type;
      Address     : in     Address_Type;
      State       :    out Result_Type)
   is

      procedure Fill_Last_Chunk
        (Heap              : in out Heap_Type;
         Transaction       : in out RW_Transaction_Type'Class;
         Context           :    out Item_Context_Type;
         Item              : in     Item_Type;
         Address           : in     Address_Type;
         Written_Size      :    out IO.Blocks.Size_Type;
         Done              :    out Boolean;
         State             :    out Result_Type)
      is
         use type Info_BTrees.Result_Type;
         use type Address_Type;
         Info       : Chunk_Info_Type;
         Pos        : Info_BTrees.Count_Type;
         St         : Info_BTrees.Result_Type;
         Last_Chunk : Address_Type;
      begin
         Info_BTrees.Look_Up(Heap.Info_Tree, Transaction.Info_Transaction,
                             Address, Info, Pos, St);
         if St /= Info_BTrees.Success then
            State := Result(St);
            pragma Warnings (Off);
            return;
            pragma Warnings (On);
         end if;
         Last_Chunk := Info.Last;
         Context    := Info.Context;
         if Last_Chunk /= Address then
            Info_BTrees.Look_Up(Heap.Info_Tree, Transaction.Info_Transaction,
                                Last_Chunk, Info, Pos, St);
            if St /= Info_BTrees.Success then
               State := Result(St);
               pragma Warnings (Off);
               return;
               pragma Warnings (On);
            end if;
         end if;

         declare
            use type IO.Blocks.Size_Type;
            use type IO.Blocks.Base_Position_Type;
            Block_Length : constant Length_Type
                         := Length_Type(IO.Blocks.Block_Size);
            Last_Block   : constant Address_Type
                         := Succ(Last_Chunk, Info.Length / Block_Length);
            Offset       : constant Length_Type
                         := (Info.Length mod Block_Length);
            Block        : IO.Blocks.Block_Type;
            First_Pos    : constant IO.Blocks.Base_Position_Type
                         := IO.Blocks.Base_Position_Type
                               (Offset + Length_Type(Block'First));
            Cursor     : IO.Blocks.Cursor_Type
                       := IO.Blocks.New_Cursor(First_Pos);
         begin
            IO_Buffers.Read(Heap.File, Transaction.Buffer, Last_Block, Block);
            loop
               Write_Part_Of_Item(Context, Block, Cursor, Item, Done);
               exit when not IO.Blocks.Is_Valid(Cursor);
               Written_Size := IO.Blocks.Moved_Since(Block, Cursor,
                                                     First_Pos);
               exit when Done;
            end loop;
            IO_Buffers.Write(Heap.File, Transaction.Buffer, Last_Block,
                             IO.Blocks.To_Block(Block, Cursor));
         end;
         State := Success;
      end Fill_Last_Chunk;

      procedure Update_Info
        (Heap                     : in out Heap_Type;
         Transaction              : in out RW_Transaction_Type'Class;
         Address                  : in     Address_Type;
         Size_Added_To_Last_Chunk : in     IO.Blocks.Size_Type;
         Context                  : in     Item_Context_Type;
         State                    :    out Result_Type)
      is
         use type Info_BTrees.Result_Type;
         use type IO.Blocks.Size_Type;
         use type Address_Type;
         Info : Chunk_Info_Type;
         Pos  : Info_BTrees.Count_Type;
         St   : Info_BTrees.Result_Type;
      begin
         Info_BTrees.Delete(Heap.Info_Tree, Transaction.Info_Transaction,
                            Address, Info, Pos, St);
         if St /= Info_BTrees.Success then
            State := Result(St);
            return;
         end if;
         pragma Assert (Info.State = Used_First);

         if Info.Last = Address then
            Info.Context := Fold_Contexts(Info.Context, Context);
            Info.Length  := Info.Length + Size_Added_To_Last_Chunk;
            Info_BTrees.Insert(Heap.Info_Tree, Transaction.Info_Transaction,
                               Address, Info, Pos, St);
            State := Result(St);
         else
            Info.Context := Fold_Contexts(Info.Context, Context);
            Info_BTrees.Insert(Heap.Info_Tree, Transaction.Info_Transaction,
                               Address, Info, Pos, St);
            if St /= Info_BTrees.Success then
               State := Result(St);
               return;
            end if;

            declare
               Last      : constant Address_Type := Info.Last;
               Last_Info : Chunk_Info_Type;
            begin
               Info_BTrees.Delete(Heap.Info_Tree, Transaction.Info_Transaction,
                                  Last, Last_Info, Pos, St);
               if St /= Info_BTrees.Success then
                  State := Result(St);
                  return;
               end if;
               pragma Assert (Last_Info.State = Used_Cont);

               Last_Info.Length := Last_Info.Length + Size_Added_To_Last_Chunk;
               Info_BTrees.Insert(Heap.Info_Tree, Transaction.Info_Transaction,
                                  Last, Last_Info, Pos, St);
               State := Result(St);
            end;
         end if;
      exception
         when others =>
            State := Error;
            pragma Warnings (Off);
            raise;
            pragma Warnings (On);
      end Update_Info;

      procedure Update_Info
        (Heap                     : in out Heap_Type;
         Transaction              : in out RW_Transaction_Type'Class;
         Address                  : in     Address_Type;
         Size_Added_To_Last_Chunk : in     IO.Blocks.Size_Type;
         New_Chunk_Address        : in     Address_Type;
         Context                  : in     Item_Context_Type;
         State                    :    out Result_Type)
      is
         use type Info_BTrees.Result_Type;
         use type IO.Blocks.Size_Type;
         use type Address_Type;
         Info : Chunk_Info_Type;
         Pos  : Info_BTrees.Count_Type;
         St   : Info_BTrees.Result_Type;
      begin
         Info_BTrees.Delete(Heap.Info_Tree, Transaction.Info_Transaction,
                            Address, Info, Pos, St);
         if St /= Info_BTrees.Success then
            State := Result(St);
            return;
         end if;
         pragma Assert (Info.State = Used_First);

         if Info.Last = Address then
            Info.Context := Fold_Contexts(Info.Context, Context);
            Info.Length  := Info.Length + Size_Added_To_Last_Chunk;
            Info.Succ    := Block_IO.To_Address(New_Chunk_Address);
            Info.Last    := New_Chunk_Address;
            Info_BTrees.Insert(Heap.Info_Tree, Transaction.Info_Transaction,
                               Address, Info, Pos, St);
            State := Result(St);
         else
            Info.Context := Fold_Contexts(Info.Context, Context);
            Info.Last    := New_Chunk_Address;
            Info_BTrees.Insert(Heap.Info_Tree, Transaction.Info_Transaction,
                               Address, Info, Pos, St);
            if St /= Info_BTrees.Success then
               State := Result(St);
               return;
            end if;

            declare
               Last      : constant Address_Type := Info.Last;
               Last_Info : Chunk_Info_Type;
            begin
               Info_BTrees.Delete(Heap.Info_Tree, Transaction.Info_Transaction,
                                  Last, Last_Info, Pos, St);
               if St /= Info_BTrees.Success then
                  State := Result(St);
                  return;
               end if;
               pragma Assert (Last_Info.State = Used_Cont);

               Last_Info.Length := Last_Info.Length + Size_Added_To_Last_Chunk;
               Last_Info.Succ   := Block_IO.To_Address(New_Chunk_Address);
               Info_BTrees.Insert(Heap.Info_Tree, Transaction.Info_Transaction,
                                  Last, Last_Info, Pos, St);
               State := Result(St);
            end;
         end if;
      exception
         when others =>
            State := Error;
            pragma Warnings (Off);
            raise;
            pragma Warnings (On);
      end Update_Info;

      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      use type IO.Blocks.Size_Type;
      Context                  : Item_Context_Type;
      Item_Size_Bound          : IO.Blocks.Size_Type;
      Size_Added_To_Last_Chunk : IO.Blocks.Size_Type;
      Done                     : Boolean;
   begin
      Item_Size_Bound := Gen_Heaps.Item_Size_Bound(Item);
      Fill_Last_Chunk(Heap, Transaction, Context, Item, Address,
                      Size_Added_To_Last_Chunk, Done, State);
      if State /= Success then
         return;
      end if;
      pragma Assert (Size_Added_To_Last_Chunk <= Item_Size_Bound);

      if Done then
         Update_Info(Heap, Transaction, Address, Size_Added_To_Last_Chunk,
                     Context, State);
      else
         declare
            New_Chunk_Address : Address_Type;
         begin
            Put_Helper(Heap, Transaction, Context, Item,
                       Item_Size_Bound - Size_Added_To_Last_Chunk,
                       Used_Cont, New_Chunk_Address, State);
            if State /= Success then
               return;
            end if;
            Update_Info(Heap, Transaction, Address, Size_Added_To_Last_Chunk,
                        New_Chunk_Address, Context, State);
         end;
      end if;
   exception
      when others =>
         State := Error;
         pragma Warnings (Off);
         raise;
         pragma Warnings (On);
   end Append;


   procedure Delete
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Address     : in     Address_Type;
      State       :    out Result_Type)
   is

      procedure Delete_Info
        (Heap        : in out Heap_Type;
         Transaction : in out RW_Transaction_Type'Class;
         Address     : in     Address_Type;
         Info        :    out Chunk_Info_Type;
         State       :    out Result_Type)
      is
         use type Info_BTrees.Result_Type;
         Pos  : Info_BTrees.Count_Type;
         St   : Info_BTrees.Result_Type;
      begin
         Info_BTrees.Delete(Heap.Info_Tree, Transaction.Info_Transaction,
                            Address, Info, Pos, St);
         if St = Info_BTrees.Success and then Info.State = Free then
            State := Failure;
         else
            State := Result(St);
         end if;
      exception
         when others =>
            State := Error;
            pragma Warnings (Off);
            raise;
            pragma Warnings (On);
      end Delete_Info;

      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      Chunk_Address : Address_Type := Address;
   begin
      loop
         declare
            Info              : Chunk_Info_Type;
            Free_Chunk_Length : Length_Type;
         begin
            -- Remove from free index
            Delete_Info(Heap, Transaction, Chunk_Address, Info, State);
            if State /= Success then
               return;
            end if;
            pragma Assert (Info.State = Used_First or Info.State = Used_Cont);

            Free_Chunk_Length := Chunk_Length(Info.Length);

            -- Prepare merge with next blocks
            Unset_Free_Following(Heap              => Heap,
                                 Transaction       => Transaction,
                                 Address           => Chunk_Address,
                                 Reverse_Direction => False,
                                 Length            => Free_Chunk_Length,
                                 State             => State);
            if State /= Success then
               return;
            end if;

            -- Prepare merge with previous blocks
            Unset_Free_Following(Heap              => Heap,
                                 Transaction       => Transaction,
                                 Address           => Chunk_Address,
                                 Reverse_Direction => True,
                                 Length            => Free_Chunk_Length,
                                 State             => State);
            if State /= Success then
               return;
            end if;

            -- Finally set as free
            Set_Free(Heap, Transaction, Chunk_Address, Free_Chunk_Length,
                     State);

            exit when not Block_IO.Is_Valid_Address(Info.Succ);
            Chunk_Address := Block_IO.To_Valid_Address(Info.Succ);
         end;
      end loop;
   exception
      when others =>
         State := Error;
         pragma Warnings (Off);
         raise;
         pragma Warnings (On);
   end Delete;


   procedure Print_Info_Index
     (Heap : in out Heap_Type)
   is
      pragma Assert (Heap.Initialized);
      LB     : constant Info_BTrees.Bound_Type
             := Info_BTrees.Negative_Infinity_Bound;
      UB     : constant Info_BTrees.Bound_Type
             := Info_BTrees.Positive_Infinity_Bound;
      Trans  : Info_BTrees.RO_Transaction_Type
             := Info_BTrees.New_RO_Transaction(Heap.Info_Tree);
      Cursor : Info_BTrees.Cursor_Type
             := Info_BTrees.New_Cursor(Tree              => Heap.Info_Tree,
                                       Thread_Safe       => False,
                                       Transaction       => Trans,
                                       Lower_Bound       => LB,
                                       Upper_Bound       => UB,
                                       Reverse_Direction => False);
      Address : Address_Type;
      Info    : Chunk_Info_Type;
      State   : Info_BTrees.Result_Type;
      use type Info_BTrees.Result_Type;
   begin
      DB.Utils.Print("INFO INDEX");
      Info_BTrees.Start_Transaction(Heap.Info_Tree, Trans);
      for I in Positive'Range loop
         Info_BTrees.Next(Heap.Info_Tree, Trans, Cursor, Address, Info, State);
         if State = Info_BTrees.Success then
            DB.Utils.Print(Positive'Image(I) &
                           Block_IO.Image(Address) &" "&
                           Chunk_State_Type'Image(Info.State) &
                           Length_Type'Image(Info.Length));
         else
            DB.Utils.Print("Finished with State = "&
                           Info_BTrees.Result_Type'Image(State));
            Info_BTrees.Finalize(Heap.Info_Tree, Cursor);
            return;
         end if;
      end loop;
      Info_BTrees.Finish_Transaction(Heap.Info_Tree, Trans);
   exception
      when others =>
         Info_BTrees.Finish_Transaction(Heap.Info_Tree, Trans);
         raise;
   end Print_Info_Index;


   procedure Print_Free_Index
     (Heap : in out Heap_Type)
   is
      pragma Assert (Heap.Initialized);
      LB     : constant Free_BTrees.Bound_Type
             := Free_BTrees.Negative_Infinity_Bound;
      UB     : constant Free_BTrees.Bound_Type
             := Free_BTrees.Positive_Infinity_Bound;
      Trans  : Free_BTrees.RO_Transaction_Type
             := Free_BTrees.New_RO_Transaction(Heap.Free_Tree);
      Cursor : Free_BTrees.Cursor_Type
             := Free_BTrees.New_Cursor(Tree              => Heap.Free_Tree,
                                       Thread_Safe       => False,
                                       Transaction       => Trans,
                                       Lower_Bound       => LB,
                                       Upper_Bound       => UB,
                                       Reverse_Direction => False);
      Length  : Length_Type;
      Address : Address_Type;
      Key     : Free_BTree_Types.Key_Type;
      Value   : Free_BTree_Types.Value_Type;
      State   : Free_BTrees.Result_Type;
      use type Free_BTrees.Result_Type;
   begin
      DB.Utils.Print("FREE INDEX");
      Free_BTrees.Start_Transaction(Heap.Free_Tree, Trans);
      for I in Positive'Range loop
         Free_BTrees.Next(Heap.Free_Tree, Trans, Cursor, Key, Value, State);
         Length  := Key.Length;
         Address := Key.Address;
         if State = Free_BTrees.Success then
            DB.Utils.Print(Positive'Image(I) &
                           Length_Type'Image(Length) &
                           Block_IO.Image(Address));
         else
            DB.Utils.Print("Finished with State = "&
                           Free_BTrees.Result_Type'Image(State));
            Free_BTrees.Finalize(Heap.Free_Tree, Cursor);
            return;
         end if;
      end loop;
      Free_BTrees.Finish_Transaction(Heap.Free_Tree, Trans);
   exception
      when others =>
         Free_BTrees.Finish_Transaction(Heap.Free_Tree, Trans);
         raise;
   end Print_Free_Index;

end DB.Gen_Heaps;

