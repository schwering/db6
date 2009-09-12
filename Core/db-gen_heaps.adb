with Ada.Text_IO; use Ada.Text_IO;
package body DB.Gen_Heaps is

   package body Info_BTree_Types is

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
      is begin
         return Block_IO."="(A, B);
      end "=";


      function "<=" (A, B : Key_Type) return Boolean
      is begin
         pragma Warnings (Off); -- the reversed order is intended
         return not Block_IO."<"(B, A);
         pragma Warnings (On);
      end "<=";

   end Info_BTree_Types;


   package body Free_BTree_Types is

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


   function Block_Identity
     (Block : IO.Blocks.Block_Type)
      return IO.Blocks.Block_Type
   is begin
      return Block;
   end Block_Identity;


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
      return Length_Type
   is begin
      return Chunk_Length(Length_Type(Size));
   end Chunk_Length;


   function Succ
     (Address : Block_IO.Valid_Address_Type;
      Count   : Length_Type)
      return Block_IO.Valid_Address_Type
   is
      Succ_Address : Block_IO.Valid_Address_Type := Address;
   begin
      for I in 1 .. Count loop
         Succ_Address := Block_IO.Succ(Succ_Address);
      end loop;
      return Succ_Address;
   end Succ;


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
            Address : Block_IO.Valid_Address_Type;
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
     (Heap : in out Heap_Type)
   is begin
      if Heap.Initialized then
         Block_IO.Close(Heap.File);
         Info_BTrees.Finalize(Heap.Info_Tree);
         Free_BTrees.Finalize(Heap.Free_Tree);
         Heap.Initialized := False;
      end if;
      Heap.Finalized := True;
   end Finalize;


   function New_RO_Transaction
     (Heap : Heap_Type)
      return RO_Transaction_Type
   is
      pragma Assert (Heap.Initialized);
   begin
      return
         (Owning_Heap      => Heap.Self,
          Initialized      => True,
          Info_Transaction => Info_BTrees.New_RO_Transaction(Heap.Info_Tree),
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
      return
         (Owning_Heap      => Heap.Self,
          Initialized      => True,
          Info_Transaction => Info_BTrees.New_RW_Transaction(Heap.Info_Tree),
          Free_Transaction => Free_BTrees.New_RW_Transaction(Heap.Free_Tree),
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


   function Min (A, B : IO.Blocks.Size_Type) return IO.Blocks.Size_Type
   is
      use type IO.Blocks.Size_Type;
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Min;


   function Data
     (Block    : IO.Blocks.Block_Type;
      Max_Size : IO.Blocks.Size_Type)
      return System.Storage_Elements.Storage_Array
   is
      Size : constant IO.Blocks.Size_Type
           := Min(Max_Size, IO.Blocks.Block_Size);
      subtype Range_Type is IO.Blocks.Size_Type range 1 .. Size;
      type Definite_Base_Block_Type is
         new System.Storage_Elements.Storage_Array(Range_Type);
      procedure Read is new IO.Blocks.Read
         (Item_Type => Definite_Base_Block_Type);
      Arr    : Definite_Base_Block_Type;
      Cursor : IO.Blocks.Cursor_Type;
   begin
      Read(Block, Cursor, Arr);
      return System.Storage_Elements.Storage_Array(Arr);
   end Data;


   procedure Set_Data
     (Block  : in out IO.Blocks.Block_Type;
      Arr    : in     System.Storage_Elements.Storage_Array;
      From   : in     IO.Blocks.Size_Type;
      Length :    out IO.Blocks.Size_Type)
   is
      use type IO.Blocks.Size_Type;
   begin
      Length := Min(Arr'Last - From + 1, IO.Blocks.Block_Size);
      declare
         subtype Range_Type is IO.Blocks.Size_Type range From .. From+Length-1;
         type Definite_Base_Block_Type is
            new System.Storage_Elements.Storage_Array(Range_Type);
         procedure Write is new IO.Blocks.Write
            (Item_Type => Definite_Base_Block_Type);
         Cursor : IO.Blocks.Cursor_Type;
      begin
         Write(Block, Cursor, Definite_Base_Block_Type(Arr(Range_Type)));
      end;
   end Set_Data;


   function Result
     (State : Info_BTrees.Result_Type)
      return Result_Type
   is begin
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
      return Result_Type
   is begin
      case State is
         when Free_BTrees.Success =>
            return Success;
         when Free_BTrees.Failure =>
            return Failure;
         when Free_BTrees.Error =>
            return Error;
      end case;
   end Result;


   procedure Get
     (Heap    : in out Heap_Type;
      Address : in     Block_IO.Valid_Address_Type;
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


   procedure Get
     (Heap        : in out Heap_Type;
      Transaction : in out Transaction_Type'Class;
      Address     : in     Block_IO.Valid_Address_Type;
      Item        :    out Item_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      Length : IO.Blocks.Size_Type;
   begin
      declare
         use type Info_BTrees.Result_Type;
         Pos : Info_BTrees.Count_Type;
         St  : Info_BTrees.Result_Type;
         CI  : Chunk_Info_Type;
      begin
         if Transaction in RW_Transaction_Type'Class then
            Info_BTrees.Look_Up(Heap.Info_Tree,
                              RW_Transaction_Type(Transaction).Info_Transaction,
                              Address, CI, Pos, St);
         else
            Info_BTrees.Look_Up(Heap.Info_Tree,
                              RO_Transaction_Type(Transaction).Info_Transaction,
                              Address, CI, Pos, St);
         end if;
         if St /= Info_BTrees.Success or else CI.State /= Used then
            State := Error;
            return;
         end if;
         Length := IO.Blocks.Size_Type(CI.Length);
      end;

      declare
         use type IO.Blocks.Size_Type;
         Addr : Block_IO.Valid_Address_Type := Address;
         Arr  : System.Storage_Elements.Storage_Array(1 .. Length);
      begin
         while Length > 0 loop
            declare
               Block : IO.Blocks.Block_Type;
            begin
               if Transaction in RW_Transaction_Type'Class then
                  IO_Buffers.Read(Heap.File,
                                  RW_Transaction_Type(Transaction).Buffer,
                                  Addr, Block);
               else
                  Block_IO.Read(Heap.File, Addr, Block);
               end if;
               declare
                  Sub_Arr : constant System.Storage_Elements.Storage_Array
                          := Data(Block, Length);
                  From    : constant IO.Blocks.Size_Type
                          := Arr'First + Arr'Length - Length;
                  To      : constant IO.Blocks.Size_Type
                          := From + Sub_Arr'Length - 1;
               begin
                  Arr(From .. To) := Sub_Arr;
                  Length          := Length - Sub_Arr'Length;
               end;
               Addr := Block_IO.Succ(Addr);
            end;
         end loop;
         Item  := From_Storage_Array(Arr);
         State := Success;
      end;

   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Get;


   procedure Put
     (Heap    : in out Heap_Type;
      Item    : in     Item_Type;
      Address :    out Block_IO.Valid_Address_Type;
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


   procedure Set_Used
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Address     : in     Block_IO.Valid_Address_Type;
      Length      : in     IO.Blocks.Size_Type;
      State       :    out Result_Type)
   is
      use type Info_BTrees.Result_Type;
      Info : constant Chunk_Info_Type := (Used, Length_Type(Length));
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
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Set_Used;


   procedure Unset_Used
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Address     : in     Block_IO.Valid_Address_Type;
      Length      :    out Length_Type;
      State       :    out Result_Type)
   is
      use type Info_BTrees.Result_Type;
      Info : Chunk_Info_Type;
      Pos  : Info_BTrees.Count_Type;
      St   : Info_BTrees.Result_Type;
   begin
      Info_BTrees.Delete(Heap.Info_Tree, Transaction.Info_Transaction,
                         Address, Info, Pos, St);
      if St /= Info_BTrees.Success then
         State  := Result(St);
         Length := 0;
         return;
      end if;
      Length := Info.Length;
      State  := Success;
   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Unset_Used;


   procedure Set_Free
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Address     : in     Block_IO.Valid_Address_Type;
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
         Info : constant Chunk_Info_Type := (Free, Length);
         Pos  : Info_BTrees.Count_Type;
         St   : Info_BTrees.Result_Type;
      begin
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
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Set_Free;


   procedure Unset_Free
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Address     : in     Block_IO.Valid_Address_Type;
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
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Unset_Free;


   procedure Unset_Free_Following
     (Heap              : in out Heap_Type;
      Transaction       : in out RW_Transaction_Type'Class;
      Address           : in out Block_IO.Valid_Address_Type;
      Reverse_Direction : in     Boolean;
      Length            : in out Length_Type;
      State             :    out Result_Type)
   is

      function Lower_Bound
        (Address           : Block_IO.Valid_Address_Type;
         Reverse_Direction : Boolean)
         return Info_BTrees.Bound_Type
      is begin
         if not Reverse_Direction then
            return Info_BTrees.New_Bound(Info_BTrees.Greater, Address);
         else
            return Info_BTrees.Negative_Infinity_Bound;
         end if;
      end Lower_Bound;

      function Upper_Bound
        (Address           : Block_IO.Valid_Address_Type;
         Reverse_Direction : Boolean)
         return Info_BTrees.Bound_Type
      is begin
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
            Addr : Block_IO.Valid_Address_Type;
            Info : Chunk_Info_Type;
            St   : Info_BTrees.Result_Type;
         begin
            Info_BTrees.Next(Heap.Info_Tree, Transaction.Info_Transaction,
                             Cursor, Addr, Info, St);
            if St /= Info_BTrees.Success or else Info.State = Used then
               State := Success;
               Info_BTrees.Finalize(Heap.Info_Tree, Cursor);
               return;
            end if;
            if Reverse_Direction then
               Address := Addr;
            end if;
            Length := Length + Info.Length;

            Info_BTrees.Delete(Heap.Info_Tree, Transaction.Info_Transaction,
                               Cursor, St);
            if St /= Info_BTrees.Success then
               State := Error;
               Info_BTrees.Finalize(Heap.Info_Tree, Cursor);
               return;
            end if;

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


   procedure Put
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Item        : in     Item_Type;
      Address     :    out Block_IO.Valid_Address_Type;
      State       :    out Result_Type)
   is

      procedure Recycle_Free_Chunk
        (Heap        : in out Heap_Type;
         Transaction : in out RW_Transaction_Type'Class;
         Min_Length  : in     Length_Type;
         Address     :    out Block_IO.Valid_Address_Type;
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

      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      Arr     : constant System.Storage_Elements.Storage_Array
              := To_Storage_Array(Item);
      Length  : constant IO.Blocks.Size_Type                   := Arr'Length;
   begin
      declare
         Chunk_Length : Length_Type;
      begin
         Recycle_Free_Chunk(Heap, Transaction, Length_Type(Length),
                            Address, Chunk_Length, State);
         case State is
            when Success =>
               -- Merge the clipping with the next free chunks
               declare
                  use type IO.Blocks.Size_Type;
                  Needed_Chunk_Length : constant Length_Type
                                      := Gen_Heaps.Chunk_Length(Length);
                  Block_Count         : constant Length_Type
                                      := Needed_Chunk_Length /
                                         Length_Type(IO.Blocks.Block_Size);
                  Free_Chunk_Length   : Length_Type
                                      := Chunk_Length - Needed_Chunk_Length;
                  Free_Address        : Block_IO.Valid_Address_Type
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
                     Set_Free(Heap, Transaction, Free_Address,
                              Free_Chunk_Length, State);
                     if State /= Success then
                        State := Error;
                        return;
                     end if;
                  end if;
               end;
            when Failure =>
               IO_Buffers.Seek_New(Heap.File, Transaction.Buffer, Address);
            when Error =>
               return;
         end case;
      end;

      Set_Used(Heap, Transaction, Address, Length, State);
      if State /= Success then
         State := Error;
         return;
      end if;
      declare
         use type IO.Blocks.Size_Type;
         Block_Address    : Block_IO.Valid_Address_Type := Address;
         Remaining_Length : IO.Blocks.Size_Type         := Arr'Length;
      begin
         while Remaining_Length > 0 loop
            declare
               Block  : IO.Blocks.Block_Type;
               Length : IO.Blocks.Size_Type;
            begin
               Set_Data(Block, Arr, Arr'First + Arr'Length - Remaining_Length,
                        Length);
               Remaining_Length := Remaining_Length - Length;
               IO_Buffers.Write(Heap.File, Transaction.Buffer, Block_Address,
                                Block);
               Block_Address := Block_IO.Succ(Block_Address);
            end;
         end loop;
      end;
      State := Success;
   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
   end Put;


   procedure Delete
     (Heap    : in out Heap_Type;
      Address : in     Block_IO.Valid_Address_Type;
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


   procedure Delete
     (Heap        : in out Heap_Type;
      Transaction : in out RW_Transaction_Type'Class;
      Address     : in     Block_IO.Valid_Address_Type;
      State       :    out Result_Type)
   is
      pragma Assert (Heap.Initialized);
      pragma Assert (Transaction.Owning_Heap = Heap.Self);
      Chunk_Address : Block_IO.Valid_Address_Type := Address;
      Length        : Length_Type;
   begin
      -- Remove from free index
      Unset_Used(Heap, Transaction, Address, Length, State);
      if State /= Success then
         return;
      end if;
      Length := Chunk_Length(Length);
      -- Prepare merge with next blocks
      Unset_Free_Following(Heap              => Heap,
                           Transaction       => Transaction,
                           Address           => Chunk_Address,
                           Reverse_Direction => False,
                           Length            => Length,
                           State             => State);
      if State /= Success then
         return;
      end if;
      -- Prepare merge with previous blocks
      Unset_Free_Following(Heap              => Heap,
                           Transaction       => Transaction,
                           Address           => Chunk_Address,
                           Reverse_Direction => True,
                           Length            => Length,
                           State             => State);
      if State /= Success then
         return;
      end if;
      -- Finally set as free
      Set_Free(Heap, Transaction, Chunk_Address, Length, State);
   exception
      when others =>
         pragma Warnings (Off);
         State := Error;
         pragma Warnings (On);
         raise;
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
      Address : Block_IO.Valid_Address_Type;
      Info    : Chunk_Info_Type;
      State   : Info_BTrees.Result_Type;
      use type Info_BTrees.Result_Type;
   begin
      Put_Line("INFO INDEX");
      Info_BTrees.Start_Transaction(Heap.Info_Tree, Trans);
      for I in Positive'Range loop
         Info_BTrees.Next(Heap.Info_Tree, Trans, Cursor, Address, Info, State);
         if State = Info_BTrees.Success then
            Put_Line(Positive'Image(I) &
                     Block_IO.Image(Address) &" "&
                     Chunk_State_Type'Image(Info.State) &
                     Length_Type'Image(Info.Length));
         else
            Put_Line("Finished with State = "&
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
      Address : Block_IO.Valid_Address_Type;
      Key     : Free_BTree_Types.Key_Type;
      Value   : Free_BTree_Types.Value_Type;
      State   : Free_BTrees.Result_Type;
      use type Free_BTrees.Result_Type;
   begin
      Put_Line("FREE INDEX");
      Free_BTrees.Start_Transaction(Heap.Free_Tree, Trans);
      for I in Positive'Range loop
         Free_BTrees.Next(Heap.Free_Tree, Trans, Cursor, Key, Value, State);
         Length  := Key.Length;
         Address := Key.Address;
         if State = Free_BTrees.Success then
            Put_Line(Positive'Image(I) &
                     Length_Type'Image(Length) &
                     Block_IO.Image(Address));
         else
            Put_Line("Finished with State = "&
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

