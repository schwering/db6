-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils.Binary_Search;
with DB.Utils.Gen_Minimum;
with DB.Utils.Gen_Maximum;

separate (DB.Gen_BTrees)
package body Nodes is

   -- The layout of a Block, i.e. a byte sequence is the following:
   -- 1. Meta_Data
   -- If Degree(N) > 0:
   -- 2. If Is_Leaf(N): Degree(N) times Index_Type (end position of entry)
   --                   where an entry is either (Key, Child) or (Key, Value)
   -- 3. If Is_Leaf(N): Key1, Value1, ..., KeyDegree(N), ValueDegree(N)
   --    Else:          Key1, Child_Addr, ..., KeyDegree(N), Child_Addr
   package Phys is
      pragma Elaborate_Body;

      function Entry_Size
        (Block : IO.Blocks.Base_Block_Type;
         Index : Valid_Index_Type)
         return IO.Blocks.Base_Position_Type;

      function Entries_Size
        (Block : IO.Blocks.Base_Block_Type)
         return IO.Blocks.Base_Position_Type;

      function Total_Size
        (Block : IO.Blocks.Base_Block_Type)
         return IO.Blocks.Base_Position_Type;

      function Effective_Block_Space
        return IO.Blocks.Base_Position_Type;

      function Max_Key_Size
        (Max_Entry_Size : IO.Blocks.Base_Position_Type;
         Max_Value_Size : IO.Blocks.Base_Position_Type)
         return IO.Blocks.Base_Position_Type;

      function Last_Used_Index
        (Block : IO.Blocks.Base_Block_Type)
         return IO.Blocks.Base_Index_Type;

      procedure Init_Block
        (Block   : in out IO.Blocks.Base_Block_Type;
         Is_Ok   : in     Boolean;
         Is_Free : in     Boolean;
         Is_Leaf : in     Boolean;
         Degree  : in     Degree_Type;
         Parent  : in     Address_Type;
         Left    : in     Address_Type;
         Right   : in     Address_Type);

      function Is_Ok
        (Block : IO.Blocks.Base_Block_Type)
         return Boolean;

      procedure Set_Ok
        (Block : in out IO.Blocks.Base_Block_Type;
         Is_Ok : in     Boolean);

      function Is_Free
        (Block : IO.Blocks.Base_Block_Type)
         return Boolean;

      function Is_Leaf
        (Block : IO.Blocks.Base_Block_Type)
         return Boolean;

      function Degree
        (Block : IO.Blocks.Base_Block_Type)
         return Degree_Type;

      function Parent
        (Block : IO.Blocks.Base_Block_Type)
         return Address_Type;

      procedure Set_Parent
        (Block   : in out IO.Blocks.Base_Block_Type;
         Address : in     Address_Type);

      function Left
        (Block : IO.Blocks.Base_Block_Type)
         return Address_Type;

      procedure Set_Left
        (Block   : in out IO.Blocks.Base_Block_Type;
         Address : in     Address_Type);

      function Right
        (Block : IO.Blocks.Base_Block_Type)
         return Address_Type;

      procedure Set_Right
        (Block   : in out IO.Blocks.Base_Block_Type;
         Address : in     Address_Type);

      procedure Read_Key
        (Key_Context : in out Key_Context_Type;
         Block       : in     IO.Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Key_Type;
         Success     :    out Boolean);

      procedure Read_Child
        (Key_Context : in out Key_Context_Type;
         Block       : in     IO.Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Child       :    out Valid_Address_Type;
         Success     :    out Boolean);

      procedure Read_Value
        (Key_Context   : in out Key_Context_Type;
         Value_Context : in out Value_Context_Type;
         Block         : in     IO.Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Value         :    out Value_Type;
         Success       :    out Boolean);

      procedure Read_Entry
        (Key_Context : in out Key_Context_Type;
         Block       : in     IO.Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Key_Type;
         Child       :    out Valid_Address_Type;
         Success     :    out Boolean);

      procedure Read_Entry
        (Key_Context   : in out Key_Context_Type;
         Value_Context : in out Value_Context_Type;
         Block         : in     IO.Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           :    out Key_Type;
         Value         :    out Value_Type;
         Success       :    out Boolean);

      procedure Write_Entry
        (Key_Context : in out Key_Context_Type;
         Block       : in out IO.Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         : in     Key_Type;
         Child       : in     Valid_Address_Type);

      procedure Write_Entry
        (Key_Context   : in out Key_Context_Type;
         Value_Context : in out Value_Context_Type;
         Block         : in out IO.Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           : in     Key_Type;
         Value         : in     Value_Type);

      procedure Write_Child
        (Key_Context : in out Key_Context_Type;
         Block       : in out IO.Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Child       : in     Valid_Address_Type);

      private
         pragma Inline (Entry_Size);
         pragma Inline (Entries_Size);
         pragma Inline (Total_Size);
         pragma Inline (Effective_Block_Space);
         pragma Inline (Last_Used_Index);
         pragma Inline (Init_Block);
         pragma Inline (Is_Ok);
         pragma Inline (Is_Free);
         pragma Inline (Degree);
         pragma Inline (Is_Leaf);
         pragma Inline (Parent);
         pragma Inline (Left);
         pragma Inline (Right);
         pragma Inline (Read_Key);
         pragma Inline (Read_Child);
         pragma Inline (Read_Value);
         pragma Inline (Read_Entry);
         pragma Inline (Write_Entry);
         pragma Inline (Write_Child);

   end Phys;


   package body Phys is
      use type IO.Blocks.Base_Position_Type;

      type Booleans_Type is
         record
            Is_Ok   : Boolean;
            Is_Free : Boolean;
            Is_Leaf : Boolean;
         end record;
      pragma Pack (Booleans_Type);

      function Size_Of is new IO.Blocks.Size_Of(Booleans_Type);
      function Size_Of is new IO.Blocks.Size_Of(Degree_Type);
      function Size_Of is new IO.Blocks.Size_Of(Address_Type);
      function Size_Of is new IO.Blocks.Size_Of(IO.Blocks.Position_Type);
      function Size_Of is new IO.Blocks.Size_Of(Valid_Address_Type);

      Size_Of_Degree    : constant IO.Blocks.Base_Position_Type :=
         IO.Blocks.Base_Position_Type(Size_Of(Degree_Type'(0)));

      Size_Of_Booleans  : constant IO.Blocks.Base_Position_Type :=
         IO.Blocks.Base_Position_Type(Size_Of(Booleans_Type'(others => <>)));

      Size_Of_Address   : constant IO.Blocks.Base_Position_Type :=
         IO.Blocks.Base_Position_Type(Size_Of(Invalid_Address));

      Size_Of_Position  : constant  IO.Blocks.Base_Position_Type :=
         IO.Blocks.Base_Position_Type
            (Size_Of(IO.Blocks.Position_Type'Last));

      Size_Of_Child     : constant IO.Blocks.Base_Position_Type :=
         IO.Blocks.Base_Position_Type(Size_Of(Valid_Address_Type
            (Block_IO.First)));

      Size_Of_Meta_Data : constant IO.Blocks.Base_Position_Type :=
         Size_Of_Degree + Size_Of_Booleans +
         Size_Of_Address + Size_Of_Address + Size_Of_Address;

      -- Layout of Node Blocks is as follows:
      -- 1. Is_Ok/Is_Free/Is_Leaf (Size_Of_Booleans)
      -- 2. Degree                (Size_Of_Degree)
      -- 4. Parent                (Size_Of_Address)
      -- 5. Left                  (Size_Of_Address)
      -- 6. Right                 (Size_Of_Address)
      -- 7. Indexes               (|Degree| * Size_Of_Position)
      -- 8. Entries               (Size_Of(Key_1) + Size_Of_Child ..
      --                           Size_Of(Key_N) + Size_Of_Child)
      --                       or (Size_Of(Key_1) + Size_Of(Value_1) ..
      --                           Size_Of(Key_N) + Size_Of(Value_N))


      function "*"
        (I : Valid_Index_Type;
         J : IO.Blocks.Base_Index_Type)
         return IO.Blocks.Base_Index_Type
      is
         pragma Inline ("*");
      begin
         return IO.Blocks.Base_Index_Type(I) * J;
      end "*";


      function Pos_From_Pos
        (Entry_Index : Valid_Index_Type)
         return IO.Blocks.Base_Index_Type
      is
         pragma Inline (Pos_From_Pos);
      begin
         if Entry_Index = 1 then
            return Size_Of_Meta_Data + 1;
         else
            return Size_Of_Meta_Data + (Entry_Index - 1) * Size_Of_Position + 1;
         end if;
      end Pos_From_Pos;


      function Pos_To_Pos
        (Entry_Index : Valid_Index_Type)
         return IO.Blocks.Base_Index_Type
      is
         pragma Inline (Pos_To_Pos);
      begin
         return Size_Of_Meta_Data + Entry_Index * Size_Of_Position;
      end Pos_To_Pos;


      function Entry_To_Pos
        (Block       : IO.Blocks.Base_Block_Type;
         Entry_Index : Valid_Index_Type)
         return IO.Blocks.Base_Index_Type
      is
         pragma Inline (Entry_To_Pos);
         pragma Assert (not Is_Free(Block));
         pragma Assert (Entry_Index in 1 .. Degree(Block));
         procedure Read is new IO.Blocks.Read_At(IO.Blocks.Base_Index_Type);
         Index : IO.Blocks.Base_Index_Type;
      begin
         Read(Block, Pos_From_Pos(Entry_Index), Pos_To_Pos(Entry_Index),
              Index);
         pragma Assert (Index'Valid);
         return Index;
      end Entry_To_Pos;


      function Entry_From_Pos
        (Block       : IO.Blocks.Base_Block_Type;
         Entry_Index : Valid_Index_Type)
         return IO.Blocks.Base_Position_Type
      is
         pragma Inline (Entry_From_Pos);
         pragma Assert (not Is_Free(Block));
         pragma Assert (Entry_Index in 1 .. Degree(Block));
      begin
         if Entry_Index = 1 then
            return Size_Of_Meta_Data + Degree(Block) * Size_Of_Position + 1;
         else
            return Entry_To_Pos(Block, Entry_Index - 1) + 1;
         end if;
      end Entry_From_Pos;


      function New_Cursor_From
        (Block : IO.Blocks.Base_Block_Type;
         Index : Valid_Index_Type)
         return IO.Blocks.Cursor_Type
      is
         pragma Inline (New_Cursor_From);
      begin
         return IO.Blocks.New_Cursor(Entry_From_Pos(Block, Index));
      end New_Cursor_From;


      procedure Set_Entry_Size
        (Block         : in out IO.Blocks.Base_Block_Type;
         Entry_Index   : in     Valid_Index_Type;
         Raw_Data_Size : in     IO.Blocks.Base_Position_Type)
      is
         pragma Inline (Set_Entry_Size);
         pragma Assert (not Is_Free(Block));
         pragma Assert (Entry_Index in 1 .. Degree(Block));
         procedure Write is new IO.Blocks.Write_At(IO.Blocks.Base_Index_Type);
         From : constant IO.Blocks.Base_Index_Type
              := Entry_From_Pos(Block, Entry_Index);
         To   : constant IO.Blocks.Base_Index_Type
              := From + Raw_Data_Size - 1;
      begin
         pragma Assert (From'Valid);
         pragma Assert (Raw_Data_Size'Valid);
         pragma Assert (To'Valid);
         Write(Block, Pos_From_Pos(Entry_Index), Pos_To_Pos(Entry_Index), To);
         pragma Assert (Entry_To_Pos(Block, Entry_Index) = To);
         pragma Assert (Entry_Size(Block, Entry_Index) =
                        Raw_Data_Size + Size_Of_Position);
      end Set_Entry_Size;


      function Entry_Size
        (Block : IO.Blocks.Base_Block_Type;
         Index : Valid_Index_Type)
         return IO.Blocks.Base_Position_Type is
      begin
         return Entry_To_Pos(Block, Index) - Entry_From_Pos(Block, Index) + 1 +
                Size_Of_Position;
      end Entry_Size;


      function Entries_Size
        (Block : IO.Blocks.Base_Block_Type)
         return IO.Blocks.Base_Position_Type is
      begin
         if Degree(Block) = 0 then
            return 0;
         else
            return Entry_To_Pos(Block, Degree(Block)) -
                   Size_Of_Meta_Data;
         end if;
      end Entries_Size;


      function Total_Size
        (Block : IO.Blocks.Base_Block_Type)
         return IO.Blocks.Base_Position_Type is
      begin
         if Is_Free(Block) then
            return Size_Of_Meta_Data;
         else
            return Size_Of_Meta_Data + Entries_Size(Block);
         end if;
      end Total_Size;


      function Effective_Block_Space
        return IO.Blocks.Base_Position_Type is
      begin
         return IO.Blocks.Index_Type'Last - Size_Of_Meta_Data;
      end Effective_Block_Space;


      function Max_Key_Size
        (Max_Entry_Size : IO.Blocks.Base_Position_Type;
         Max_Value_Size : IO.Blocks.Base_Position_Type)
         return IO.Blocks.Base_Position_Type is
      begin
         if Size_Of_Child > Max_Value_Size then
            return Max_Entry_Size - Size_Of_Child - Size_Of_Position;
         else
            return Max_Entry_Size - Max_Value_Size - Size_Of_Position;
         end if;
      end Max_Key_Size;


      function Last_Used_Index
        (Block : IO.Blocks.Base_Block_Type)
         return IO.Blocks.Base_Index_Type is
      begin
         if Degree(Block) = 0 then
            return IO.Blocks.Base_Index_Type(Size_Of_Meta_Data);
         else
            return Entry_To_Pos(Block, Degree(Block));
         end if;
      end Last_Used_Index;


      function Free_Space
        (Block : IO.Blocks.Base_Block_Type)
         return IO.Blocks.Base_Position_Type
      is
         pragma Inline (Free_Space);
      begin
         return IO.Blocks.Base_Index_Type'Last - Last_Used_Index(Block);
      end Free_Space;
      pragma Unreferenced (Free_Space);


      function Is_Ok
        (Block : IO.Blocks.Base_Block_Type)
         return Boolean
      is
         procedure Read is new IO.Blocks.Read_At(Booleans_Type);
         Offset   : constant IO.Blocks.Base_Position_Type := 0;
         From     : constant IO.Blocks.Base_Index_Type
                  := IO.Blocks.Base_Index_Type'First + Offset;
         Booleans : Booleans_Type;
      begin
         Read(Block, From, From + Size_Of_Booleans - 1, Booleans);
         return Booleans.Is_Ok;
      end Is_Ok;


      procedure Set_Ok
        (Block : in out IO.Blocks.Base_Block_Type;
         Is_Ok : in     Boolean)
      is
         procedure Read is new IO.Blocks.Read_At(Booleans_Type);
         procedure Write is new IO.Blocks.Write_At(Booleans_Type);
         Offset   : constant IO.Blocks.Base_Position_Type := 0;
         From     : constant IO.Blocks.Base_Index_Type
                  := IO.Blocks.Base_Index_Type'First + Offset;
         Booleans : Booleans_Type;
      begin
         Read(Block, From, From + Size_Of_Booleans - 1, Booleans);
         Booleans.Is_Ok := Is_Ok;
         Write(Block, From, From + Size_Of_Booleans - 1, Booleans);
      end Set_Ok;


      function Is_Free
        (Block : IO.Blocks.Base_Block_Type)
         return Boolean
      is
         procedure Read is new IO.Blocks.Read_At(Booleans_Type);
         Offset   : constant IO.Blocks.Base_Position_Type := 0;
         From     : constant IO.Blocks.Base_Index_Type
                  := IO.Blocks.Base_Index_Type'First + Offset;
         Booleans : Booleans_Type;
      begin
         Read(Block, From, From + Size_Of_Booleans - 1, Booleans);
         return Booleans.Is_Free;
      end Is_Free;


      function Is_Leaf
        (Block : IO.Blocks.Base_Block_Type)
         return Boolean
      is
         procedure Read is new IO.Blocks.Read_At(Booleans_Type);
         Offset   : constant IO.Blocks.Base_Position_Type := 0;
         From     : constant IO.Blocks.Base_Index_Type
                  := IO.Blocks.Base_Index_Type'First + Offset;
         Booleans : Booleans_Type;
      begin
         Read(Block, From, From + Size_Of_Booleans - 1, Booleans);
         return Booleans.Is_Leaf;
      end Is_Leaf;


      function Degree
        (Block : IO.Blocks.Base_Block_Type)
         return Degree_Type
      is
         pragma Assert (not Is_Free(Block));
         procedure Read is new IO.Blocks.Read_At(Degree_Type);
         Offset : constant IO.Blocks.Base_Position_Type := Size_Of_Booleans;
         From   : constant IO.Blocks.Base_Index_Type
                := IO.Blocks.Base_Index_Type'First + Offset;
         Degree : Degree_Type;
      begin
         Read(Block, From, From + Size_Of_Degree - 1, Degree);
         pragma Assert (Degree'Valid);
         return Degree;
      end Degree;


      procedure Set_Degree
        (Block  : in out IO.Blocks.Base_Block_Type;
         Degree : in     Degree_Type)
      is
         pragma Assert (not Is_Free(Block));
         procedure Write is new IO.Blocks.Write_At(Degree_Type);
         Offset : constant IO.Blocks.Base_Position_Type := Size_Of_Booleans;
         From   : constant IO.Blocks.Base_Index_Type
                := IO.Blocks.Base_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Degree - 1, Degree);
         pragma Assert (Degree = Phys.Degree(Block));
      end Set_Degree;


      function Parent
        (Block : IO.Blocks.Base_Block_Type)
         return Address_Type
      is
         pragma Assert (not Is_Free(Block));
         procedure Read is new IO.Blocks.Read_At(Address_Type);
         Offset  : constant IO.Blocks.Base_Position_Type
                 := Size_Of_Booleans + Size_Of_Degree;
         From    : constant IO.Blocks.Base_Index_Type
                 := IO.Blocks.Base_Index_Type'First + Offset;
         Address : Address_Type;
      begin
         Read(Block, From, From + Size_Of_Address - 1, Address);
         return Address;
      end Parent;


      procedure Set_Parent
        (Block   : in out IO.Blocks.Base_Block_Type;
         Address : in     Address_Type)
      is
         pragma Assert (not Is_Free(Block));
         procedure Write is new IO.Blocks.Write_At(Address_Type);
         Offset : constant IO.Blocks.Base_Position_Type
                := Size_Of_Booleans + Size_Of_Degree;
         From   : constant IO.Blocks.Base_Index_Type
                := IO.Blocks.Base_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Address - 1, Address);
         pragma Assert (Parent(Block) = Address);
      end Set_Parent;


      function Left
        (Block : IO.Blocks.Base_Block_Type)
         return Address_Type
      is
         procedure Read is new IO.Blocks.Read_At(Address_Type);
         Offset  : constant IO.Blocks.Base_Position_Type
                 := Size_Of_Booleans + Size_Of_Degree + Size_Of_Address;
         From    : constant IO.Blocks.Base_Index_Type
                 := IO.Blocks.Base_Index_Type'First + Offset;
         Address : Address_Type;
      begin
         Read(Block, From, From + Size_Of_Address - 1, Address);
         return Address;
      end Left;


      procedure Set_Left
        (Block   : in out IO.Blocks.Base_Block_Type;
         Address : in     Address_Type)
      is
         procedure Write is new IO.Blocks.Write_At(Address_Type);
         Offset : constant IO.Blocks.Base_Position_Type
                := Size_Of_Booleans + Size_Of_Degree + Size_Of_Address;
         From   : constant IO.Blocks.Base_Index_Type
                := IO.Blocks.Base_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Address - 1, Address);
         pragma Assert (Left(Block) = Address);
      end Set_Left;


      function Right
        (Block : IO.Blocks.Base_Block_Type)
         return Address_Type
      is
         procedure Read is new IO.Blocks.Read_At(Address_Type);
         Offset  : constant IO.Blocks.Base_Position_Type
                 := Size_Of_Booleans + Size_Of_Degree + Size_Of_Address +
                    Size_Of_Address;
         From    : constant IO.Blocks.Base_Index_Type
                 := IO.Blocks.Base_Index_Type'First + Offset;
         Address : Address_Type;
      begin
         Read(Block, From, From + Size_Of_Address - 1, Address);
         return Address;
      end Right;


      procedure Set_Right
        (Block   : in out IO.Blocks.Base_Block_Type;
         Address : in     Address_Type)
      is
         procedure Write is new IO.Blocks.Write_At(Address_Type);
         Offset : constant IO.Blocks.Base_Position_Type
                := Size_Of_Booleans + Size_Of_Degree + Size_Of_Address +
                   Size_Of_Address;
         From   : constant IO.Blocks.Base_Index_Type
                := IO.Blocks.Base_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Address - 1, Address);
         pragma Assert (Right(Block) = Address);
      end Set_Right;


      procedure Init_Block
        (Block   : in out IO.Blocks.Base_Block_Type;
         Is_Ok   : in     Boolean;
         Is_Free : in     Boolean;
         Is_Leaf : in     Boolean;
         Degree  : in     Degree_Type;
         Parent  : in     Address_Type;
         Left    : in     Address_Type;
         Right   : in     Address_Type)
      is
         procedure Set_Booleans
           (Block    : in out IO.Blocks.Base_Block_Type;
            Booleans : in     Booleans_Type)
         is
            procedure Write is new IO.Blocks.Write_At(Booleans_Type);
            Offset : constant IO.Blocks.Base_Position_Type := 0;
            From   : constant IO.Blocks.Base_Index_Type
                   := IO.Blocks.Base_Index_Type'First + Offset;
         begin
            Write(Block, From, From + Size_Of_Booleans - 1, Booleans);
            pragma Assert (Phys.Is_Ok(Block) = Booleans.Is_Ok);
            pragma Assert (Phys.Is_Free(Block) = Booleans.Is_Free);
            pragma Assert (Booleans.Is_Free or else
                           Phys.Is_Leaf(Block) = Booleans.Is_Leaf);
         end Set_Booleans;

         Booleans : constant Booleans_Type
                  := Booleans_Type'(Is_Ok   => Is_Ok,
                                    Is_Free => Is_Free,
                                    Is_Leaf => Is_Leaf);
      begin
         Set_Booleans(Block, Booleans);
         if not Is_Free then
            Set_Degree(Block, Degree);
            Set_Parent(Block, Parent);
         end if;
         Set_Left(Block, Left);
         Set_Right(Block, Right);
         pragma Assert (Is_Free or else Phys.Degree(Block) = Degree);
         pragma Assert (Is_Free or else Phys.Is_Leaf(Block) = Is_Leaf);
         pragma Assert (Is_Free or else Phys.Parent(Block) = Parent);
         pragma Assert (Phys.Is_Free(Block) = Is_Free);
         pragma Assert (Phys.Left(Block) = Left);
         pragma Assert (Phys.Right(Block) = Right);
      end Init_Block;


      procedure Read_Key
        (Key_Context : in out Key_Context_Type;
         Block       : in     IO.Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Key_Type;
         Success     :    out Boolean)
      is
         Cursor : IO.Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Read_Key(Key_Context, Block, Cursor, Key);
         Success := IO.Blocks.Is_Valid(Cursor);
      end Read_Key;


      procedure Read_Child
        (Key_Context : in out Key_Context_Type;
         Block       : in     IO.Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Child       :    out Valid_Address_Type;
         Success     :    out Boolean)
      is
         pragma Assert (not Is_Leaf(Block));
         procedure Read_Child is new IO.Blocks.Read(Valid_Address_Type);
         Cursor : IO.Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Skip_Key(Key_Context, Block, Cursor);
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Read_Child(Block, Cursor, Child);
         Success := IO.Blocks.Is_Valid(Cursor);
      end Read_Child;


      procedure Read_Value
        (Key_Context   : in out Key_Context_Type;
         Value_Context : in out Value_Context_Type;
         Block         : in     IO.Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Value         :    out Value_Type;
         Success       :    out Boolean)
      is
         pragma Assert (Is_Leaf(Block));
         Cursor : IO.Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Skip_Key(Key_Context, Block, Cursor);
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Read_Value(Value_Context, Block, Cursor, Value);
         Success := IO.Blocks.Is_Valid(Cursor);
      end Read_Value;


      procedure Read_Entry
        (Key_Context : in out Key_Context_Type;
         Block       : in     IO.Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Key_Type;
         Child       :    out Valid_Address_Type;
         Success     :    out Boolean)
      is
         pragma Assert (not Is_Leaf(Block));
         procedure Read_Child is new IO.Blocks.Read(Valid_Address_Type);
         Cursor : IO.Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Read_Key(Key_Context, Block, Cursor, Key);
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Read_Child(Block, Cursor, Child);
         Success := IO.Blocks.Is_Valid(Cursor);
      end Read_Entry;


      procedure Read_Entry
        (Key_Context   : in out Key_Context_Type;
         Value_Context : in out Value_Context_Type;
         Block         : in     IO.Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           :    out Key_Type;
         Value         :    out Value_Type;
         Success       :    out Boolean)
      is
         pragma Assert (Is_Leaf(Block));
         Cursor : IO.Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Read_Key(Key_Context, Block, Cursor, Key);
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Read_Value(Value_Context, Block, Cursor, Value);
         Success := IO.Blocks.Is_Valid(Cursor);
      end Read_Entry;


      procedure Write_Entry
        (Key_Context : in out Key_Context_Type;
         Block       : in out IO.Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         : in     Key_Type;
         Child       : in     Valid_Address_Type)
      is
         pragma Assert (not Is_Leaf(Block));
         procedure Write_Child is new IO.Blocks.Write(Valid_Address_Type);
         Cursor : IO.Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         if not IO.Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Write_Key(Key_Context, Block, Cursor, Key);
         if not IO.Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Write_Child(Block, Cursor, Child);
         if not IO.Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         declare
            Size : constant IO.Blocks.Base_Position_Type
                 := IO.Blocks.Position(Cursor) - Entry_From_Pos(Block, Index);
         begin
            Set_Entry_Size(Block, Index, Size);
         end;
      end Write_Entry;


      procedure Write_Entry
        (Key_Context   : in out Key_Context_Type;
         Value_Context : in out Value_Context_Type;
         Block         : in out IO.Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           : in     Key_Type;
         Value         : in     Value_Type)
      is
         pragma Assert (Is_Leaf(Block));
         Cursor : IO.Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         if not IO.Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Write_Key(Key_Context, Block, Cursor, Key);
         if not IO.Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Write_Value(Value_Context, Block, Cursor, Value);
         if not IO.Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         declare
            Size : constant IO.Blocks.Base_Position_Type
                 := IO.Blocks.Position(Cursor) - Entry_From_Pos(Block, Index);
         begin
            Set_Entry_Size(Block, Index, Size);
         end;

         --declare
         --   Key_Context : Key_Context_Type     := New_Key_Context;
         --   Value_Context : Value_Context_Type := New_Value_Context;
         --   K : Key_Type;
         --   V : Value_Type;
         --   S : Boolean;
         --begin
         --   Read_Entry(Key_Context, Value_Context, Block, Index, K, V, S);
         --   pragma Assert (S);
         --   pragma Assert (K = Key);
         --   --pragma Assert (V = Value);
         --end;
      end Write_Entry;


      procedure Write_Child
        (Key_Context : in out Key_Context_Type;
         Block       : in out IO.Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Child       : in     Valid_Address_Type)
      is
         pragma Assert (not Is_Leaf(Block));
         procedure Write_Child is new IO.Blocks.Write(Valid_Address_Type);
         Cursor : IO.Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         if not IO.Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Skip_Key(Key_Context, Block, Cursor);
         if not IO.Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Write_Child(Block, Cursor, Child);
         if not IO.Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
      end Write_Child;

   end Phys;


   ----------
   -- Address functions.

   function Is_Valid
     (Address : Address_Type)
      return Boolean is
   begin
      return Block_IO.Is_Valid_Address(Block_IO.Address_Type(Address));
   end Is_Valid;


   function Is_Valid
     (Address : Valid_Address_Type)
      return Boolean is
   begin
      return Is_Valid(To_Address(Address));
   end Is_Valid;


   function To_Valid_Address
     (Address : Address_Type)
      return Valid_Address_Type is
   begin
      return Valid_Address_Type(
         Block_IO.To_Valid_Address(Block_IO.Address_Type(Address)));
   end To_Valid_Address;


   function To_Address
     (Address : Valid_Address_Type)
      return Address_Type is
   begin
      return Address_Type(
         Block_IO.To_Address(Block_IO.Valid_Address_Type(Address)));
   end To_Address;


   function Is_Valid
     (Index : Index_Type)
      return Boolean is
   begin
      return Index /= Invalid_Index;
   end Is_Valid;


   ----------
   -- General and accessor subprograms.

   function New_RW_Node
        (Is_Ok   : Boolean;
         Is_Free : Boolean;
         Is_Leaf : Boolean;
         Degree  : Degree_Type;
         Parent  : Address_Type;
         Left    : Address_Type;
         Right   : Address_Type)
         return RW_Node_Type
   is
      Node : RW_Node_Type;
   begin
      Phys.Init_Block(Block   => IO.Blocks.Base_Block_Type(Node),
                      Is_Ok   => Is_Ok,
                      Is_Free => Is_Free,
                      Is_Leaf => Is_Leaf,
                      Degree  => Degree,
                      Parent  => Parent,
                      Left    => Left,
                      Right   => Right);
      return Node;
   end New_RW_Node;


   function Invalid_Node
     return RW_Node_Type is
   begin
      return New_RW_Node(Is_Ok   => False,
                         Is_Free => True,
                         Is_Leaf => True,
                         Degree  => 0,
                         Parent  => Invalid_Address,
                         Left    => Invalid_Address,
                         Right   => Invalid_Address);
   end Invalid_Node;


   function Root_Node
     (Is_Leaf : Boolean)
      return RW_Node_Type is
   begin
      return New_RW_Node(Is_Ok   => True,
                         Is_Free => False,
                         Is_Leaf => Is_Leaf,
                         Degree  => 0,
                         Parent  => Invalid_Address,
                         Left    => Invalid_Address,
                         Right   => Invalid_Address);
   end Root_Node;


   function Free_Node
      return RW_Node_Type is
   begin
      return New_RW_Node(Is_Ok   => True,
                         Is_Free => True,
                         Is_Leaf => False,
                         Degree  => 0,
                         Parent  => Invalid_Address,
                         Left    => Invalid_Address,
                         Right   => Invalid_Address);
   end Free_Node;


   function Is_Ok
     (Node : Node_Type)
      return Boolean is
   begin
      return Phys.Is_Ok(IO.Blocks.Base_Block_Type(Node));
   end Is_Ok;


   function Is_Free
     (Node : Node_Type)
      return Boolean
   is
      pragma Assert (Is_Ok(Node));
   begin
      return Phys.Is_Free(IO.Blocks.Base_Block_Type(Node));
   end Is_Free;


   function Degree
     (Node : Node_Type)
      return Degree_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
   begin
      return Phys.Degree(IO.Blocks.Base_Block_Type(Node));
   end Degree;


   function Is_Leaf
     (Node : Node_Type)
      return Boolean
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
   begin
      return Phys.Is_Leaf(IO.Blocks.Base_Block_Type(Node));
   end Is_Leaf;


   function Is_Inner
     (Node : Node_Type)
      return Boolean
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
   begin
      return not Is_Leaf(Node);
   end Is_Inner;


   function Parent
     (Node : Node_Type)
      return Address_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
   begin
      return Phys.Parent(IO.Blocks.Base_Block_Type(Node));
   end Parent;


   function Valid_Parent
     (Node : Node_Type)
      return Valid_Address_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
   begin
      return To_Valid_Address(Parent(Node));
   end Valid_Parent;


   function Is_Root
     (Node : Node_Type)
      return Boolean
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
   begin
      return not Is_Valid(Left_Neighbor(Node)) and
             not Is_Valid(Right_Neighbor(Node));
   end Is_Root;


   procedure Set_Left_Neighbor
     (Node     : in out Node_Type;
      Neighbor : in     Address_Type)
   is
      pragma Assert (Is_Ok(Node));
   begin
      Phys.Set_Left(IO.Blocks.Base_Block_Type(Node), Neighbor);
   end Set_Left_Neighbor;


   procedure Set_Left_Neighbor
     (Node     : in out Node_Type;
      Neighbor : in     Valid_Address_Type)
   is
      pragma Assert (Is_Ok(Node));
   begin
      Phys.Set_Left(IO.Blocks.Base_Block_Type(Node), To_Address(Neighbor));
   end Set_Left_Neighbor;


   function Left_Neighbor
     (Node : Node_Type)
      return Address_Type
   is
      pragma Assert (Is_Ok(Node));
   begin
      return Phys.Left(IO.Blocks.Base_Block_Type(Node));
   end Left_Neighbor;


   function Valid_Left_Neighbor
     (Node : Node_Type)
      return Valid_Address_Type
   is
      pragma Assert (Is_Ok(Node));
   begin
      return To_Valid_Address(Phys.Left(IO.Blocks.Base_Block_Type(Node)));
   end Valid_Left_Neighbor;


   function Right_Neighbor
     (Node : Node_Type)
      return Address_Type
   is
      pragma Assert (Is_Ok(Node));
   begin
      return Phys.Right(IO.Blocks.Base_Block_Type(Node));
   end Right_Neighbor;


   function Valid_Right_Neighbor
     (Node : Node_Type)
      return Valid_Address_Type
   is
      pragma Assert (Is_Ok(Node));
   begin
      return To_Valid_Address(Phys.Right(IO.Blocks.Base_Block_Type(Node)));
   end Valid_Right_Neighbor;


   procedure Get_Key
     (Node        : in     Node_Type;
      Index       : in     Valid_Index_Type;
      Key         :    out Key_Type;
      Key_Context : in out Key_Context_Type)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Success : Boolean;
   begin
      Phys.Read_Key(Key_Context, IO.Blocks.Base_Block_Type(Node), Index, Key,
                    Success);
      if not Success then
         raise Node_Error;
      end if;
   end Get_Key;


   function Key
     (Node  : Node_Type;
      Index : Valid_Index_Type)
      return Key_Type
   is
      Key         : Key_Type;
      Key_Context : Key_Context_Type := New_Key_Context;
   begin
      Get_Key(Node, Index, Key, Key_Context);
      return Key;
   end Key;


   procedure Get_Child
     (Node        : in     Node_Type;
      Index       : in     Valid_Index_Type;
      Child       :    out Valid_Address_Type;
      Key_Context : in out Key_Context_Type)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Inner(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Success : Boolean;
   begin
      Phys.Read_Child(Key_Context, IO.Blocks.Base_Block_Type(Node), Index,
                      Child, Success);
      pragma Assert (Is_Valid(Child));
      if not Success then
         raise Node_Error;
      end if;
   end Get_Child;


   function Child
     (Node  : Node_Type;
      Index : Valid_Index_Type)
      return Valid_Address_Type
   is
      Child       : Valid_Address_Type;
      Key_Context : Key_Context_Type := New_Key_Context;
   begin
      Get_Child(Node, Index, Child, Key_Context);
      return Child;
   end Child;


   procedure Get_Value
     (Node          : in     Node_Type;
      Index         : in     Valid_Index_Type;
      Value         :    out Value_Type;
      Key_Context   : in out Key_Context_Type;
      Value_Context : in out Value_Context_Type)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Leaf(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Success : Boolean;
   begin
      Phys.Read_Value(Key_Context, Value_Context,
                      IO.Blocks.Base_Block_Type(Node), Index, Value, Success);
      if not Success then
         raise Node_Error;
      end if;
   end Get_Value;


   function Value
     (Node  : Node_Type;
      Index : Valid_Index_Type)
      return Value_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Leaf(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Value         : Value_Type;
      Key_Context   : Key_Context_Type := New_Key_Context;
      Value_Context : Value_Context_Type := New_Value_Context;
   begin
      Get_Value(Node, Index, Value, Key_Context, Value_Context);
      return Value;
   end Value;


   function Key_Position
     (Node : Node_Type;
      Key  : Key_Type)
      return Index_Type
   is
      Key_Context : Key_Context_Type := New_Key_Context;

      function Get_Key
        (Node  : Node_Type;
         Index : Index_Type)
         return Key_Type
      is
         Key : Key_Type;
      begin
         Get_Key(Node, Index, Key, Key_Context);
         return Key;
      end Get_Key;

      function Key_Position_Uniform_Binary
        (Node : Node_Type;
         Key  : Key_Type)
         return Index_Type
      is
         pragma Inline (Key_Position_Uniform_Binary);
         pragma Assert (Is_Ok(Node));
         pragma Assert (not Is_Free(Node));
      begin
         if Degree(Node) > 0 then
            declare
               procedure Find is
                  new Utils.Binary_Search.Uniform_Find_Best_In_Container
                        (Container_Type      => Node_Type,
                         Extended_Index_Type => Index_Type,
                         Invalid_Index       => Invalid_Index,
                         Item_Type           => Key_Type,
                         Get                 => Get_Key,
                         Compare             => Compare);
               Index : Index_Type;
            begin
               Find(Node, 1, Degree(Node), Key, Index);
               if Nodes.Is_Valid(Index) then
                  return Index;
               else
                  return Invalid_Index;
               end if;
            end;
         else
            return Invalid_Index;
         end if;
      end Key_Position_Uniform_Binary;


      function Key_Position_Binary
        (Node : Node_Type;
         Key  : Key_Type)
         return Index_Type
      is
         pragma Inline (Key_Position_Binary);
         pragma Assert (Is_Ok(Node));
         pragma Assert (not Is_Free(Node));
      begin
         if Degree(Node) > 0 then
            declare
               procedure Find is
                  new Utils.Binary_Search.Find_Best_In_Container
                        (Container_Type => Node_Type,
                         Index_Type     => Valid_Index_Type,
                         Item_Type      => Key_Type,
                         Get            => Get_Key,
                         "<="           => "<=");
               Index : Index_Type;
               Found : Boolean;
            begin
               Find(Node, 1, Degree(Node), Key, Found, Index);
               if Found then
                  return Index;
               else
                  return Invalid_Index;
               end if;
            end;
         else
            return Invalid_Index;
         end if;
      end Key_Position_Binary;


      function Key_Position_Linear
        (Node : Node_Type;
         Key  : Key_Type)
         return Index_Type
      is
         pragma Inline (Key_Position_Linear);
         pragma Assert (Is_Ok(Node));
         pragma Assert (not Is_Free(Node));
      begin
         for I in 1 .. Degree(Node) loop
            if Key <= Nodes.Key(Node, I) then
               return I;
            end if;
         end loop;
         return Invalid_Index;
      end Key_Position_Linear;

      pragma Unreferenced (Key_Position_Binary);
      pragma Unreferenced (Key_Position_Linear);
   begin
      return Key_Position_Uniform_Binary(Node, Key);
   end Key_Position;


   function Child_Position
     (Node  : Node_Type;
      Child : Valid_Address_Type)
      return Valid_Index_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
   begin
      for I in 1 .. Degree(Node) loop
         if Child = Nodes.Child(Node, I) then
            return I;
         end if;
      end loop;
      raise Node_Error;
   end Child_Position;


   ----------
   -- Node operations.

   procedure Set_Parent
     (Node   : in out RW_Node_Type;
      Parent : in     Address_Type)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
   begin
      Phys.Set_Parent(IO.Blocks.Base_Block_Type(Node), Parent);
   end Set_Parent;


   procedure Set_Parent
     (Node   : in out RW_Node_Type;
      Parent : in     Valid_Address_Type)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
   begin
      Phys.Set_Parent(IO.Blocks.Base_Block_Type(Node), To_Address(Parent));
   end Set_Parent;


   procedure Set_Right_Neighbor
     (Node     : in out RW_Node_Type;
      Neighbor : in     Address_Type)
   is
      pragma Assert (Is_Ok(Node));
   begin
      Phys.Set_Right(IO.Blocks.Base_Block_Type(Node), Neighbor);
   end Set_Right_Neighbor;


   procedure Set_Right_Neighbor
     (Node     : in out RW_Node_Type;
      Neighbor : in     Valid_Address_Type)
   is
      pragma Assert (Is_Ok(Node));
   begin
      Phys.Set_Right(IO.Blocks.Base_Block_Type(Node), To_Address(Neighbor));
   end Set_Right_Neighbor;


   function Split_Position
     (Node : RW_Node_Type)
      return Valid_Index_Type
   is
      use type IO.Blocks.Base_Position_Type;

      function Min is new Utils.Gen_Minimum(IO.Blocks.Base_Position_Type);
      pragma Inline (Min);

      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));

      Total    : constant IO.Blocks.Base_Position_Type
               := Phys.Entries_Size(IO.Blocks.Base_Block_Type(Node));
      Sum      : IO.Blocks.Base_Position_Type := 0;
      Prev_Min : IO.Blocks.Base_Position_Type := 0;
   begin
      for I in 1 .. Degree(Node) loop
         Sum := Sum + Phys.Entry_Size(IO.Blocks.Base_Block_Type(Node), I);
         declare
            This_Min : constant IO.Blocks.Base_Position_Type
                     := Min(Sum, Total - Sum);
         begin
            if Prev_Min > This_Min then
               return I;
            end if;
            Prev_Min := This_Min;
         end;
      end loop;
      raise Tree_Error; -- never reached
   end Split_Position;


   function Combi_Split_Position
     (Left_Node  : RW_Node_Type;
      Right_Node : RW_Node_Type)
      return Valid_Index_Type
   is
      use type IO.Blocks.Size_Type;

      function Min is new Utils.Gen_Minimum(IO.Blocks.Size_Type);
      pragma Inline (Min);

      pragma Assert (Is_Ok(Left_Node));
      pragma Assert (Is_Ok(Right_Node));
      pragma Assert (not Is_Free(Left_Node));
      pragma Assert (not Is_Free(Right_Node));

      Total    : constant IO.Blocks.Size_Type
               := IO.Blocks.Size_Type(Phys.Entries_Size
                                       (IO.Blocks.Base_Block_Type(Left_Node))) +
                  IO.Blocks.Size_Type(Phys.Entries_Size
                                       (IO.Blocks.Base_Block_Type(Right_Node)));
      Sum      : IO.Blocks.Size_Type := 0;
      Prev_Min : IO.Blocks.Size_Type := 0;
   begin
      for I in 1 .. Degree(Left_Node) loop
         Sum := Sum + IO.Blocks.Size_Type(Phys.Entry_Size
                                    (IO.Blocks.Base_Block_Type(Left_Node), I));
         declare
            This_Min : constant IO.Blocks.Size_Type
                     := Min(Sum, Total - Sum);
         begin
            if Prev_Min > This_Min then
               return I;
            end if;
            Prev_Min := This_Min;
         end;
      end loop;
      for I in 1 .. Degree(Right_Node) loop
         Sum := Sum + IO.Blocks.Size_Type(Phys.Entry_Size
                                    (IO.Blocks.Base_Block_Type(Right_Node), I));
         declare
            This_Min : constant IO.Blocks.Size_Type
                     := Min(Sum, Total - Sum);
         begin
            if Prev_Min > This_Min then
               return Degree(Left_Node) + I;
            end if;
            Prev_Min := This_Min;
         end;
      end loop;
      raise Tree_Error; -- never reached
   end Combi_Split_Position;


   procedure Set_Child
     (Node  : in out RW_Node_Type;
      Index : in     Valid_Index_Type;
      Child : in     Valid_Address_Type)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Index in 1 .. Degree(Node));
      pragma Assert (Is_Valid(Child));

      Key_Context : Key_Context_Type := New_Key_Context;
   begin
      Phys.Write_Child(Key_Context, IO.Blocks.Base_Block_Type(Node), Index,
                       Child);
      if not Nodes.Is_Ok(Node) then
         raise Node_Error;
      end if;
   end Set_Child;


   procedure Copy_Entry
     (Node              : in out RW_Node_Type;
      Source            : in     RW_Node_Type;
      Index             : in     Valid_Index_Type;
      Key_Read_Context  : in out Key_Context_Type;
      Key_Write_Context : in out Key_Context_Type;
      Shift_By          : in     Integer := 0)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Inner(Node));

      New_Index : constant Valid_Index_Type
                := Valid_Index_Type(Integer(Index) + Shift_By);
      Key       : Key_Type;
      Child     : Valid_Address_Type;
      Success   : Boolean;
   begin
      Phys.Read_Entry(Key_Read_Context, IO.Blocks.Base_Block_Type(Source),
                      Index, Key, Child, Success);
      if not Success then
         raise Node_Error;
      end if;
      pragma Assert (Is_Valid(Child));
      Phys.Write_Entry(Key_Write_Context, IO.Blocks.Base_Block_Type(Node),
                       New_Index, Key, Child);
   end Copy_Entry;


   procedure Copy_Entry
     (Node                : in out RW_Node_Type;
      Source              : in     RW_Node_Type;
      Index               : in     Valid_Index_Type;
      Key_Read_Context    : in out Key_Context_Type;
      Key_Write_Context   : in out Key_Context_Type;
      Value_Read_Context  : in out Value_Context_Type;
      Value_Write_Context : in out Value_Context_Type;
      Shift_By            : in     Integer := 0)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Leaf(Node));

      New_Index : constant Valid_Index_Type
                := Valid_Index_Type(Integer(Index) + Shift_By);
      Key       : Key_Type;
      Value     : Value_Type;
      Success   : Boolean;
   begin
      Phys.Read_Entry(Key_Read_Context, Value_Read_Context,
                      IO.Blocks.Base_Block_Type(Source), Index, Key, Value,
                      Success);
      if not Success then
         raise Node_Error;
      end if;
      Phys.Write_Entry(Key_Write_Context, Value_Write_Context,
                       IO.Blocks.Base_Block_Type(Node), New_Index, Key, Value);
   end Copy_Entry;


   function Insertion
     (Node  : RW_Node_Type;
      Index : Valid_Index_Type;
      Key   : Key_Type;
      Child : Valid_Address_Type)
      return RW_Node_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Inner(Node));

      N : RW_Node_Type := New_RW_Node(Is_Ok   => True,
                                      Is_Free => Is_Free(Node),
                                      Is_Leaf => Is_Leaf(Node),
                                      Degree  => Degree(Node) + 1,
                                      Parent  => Parent(Node),
                                      Left    => Left_Neighbor(Node),
                                      Right   => Right_Neighbor(Node));
      Key_Read_Context  : Key_Context_Type := New_Key_Context;
      Key_Write_Context : Key_Context_Type := New_Key_Context;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, IO.Blocks.Base_Block_Type(N), Index,
                       Key, Child);
      if not Is_Ok(N) then
         return N;
      end if;

      for I in Index .. Degree(Node) loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context, +1);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;
      return N;
   end Insertion;


   function Insertion
     (Node  : RW_Node_Type;
      Index : Valid_Index_Type;
      Key   : Key_Type;
      Value : Value_Type)
      return RW_Node_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Leaf(Node));

      N : RW_Node_Type := New_RW_Node(Is_Ok   => True,
                                      Is_Free => Is_Free(Node),
                                      Is_Leaf => Is_Leaf(Node),
                                      Degree  => Degree(Node) + 1,
                                      Parent  => Parent(Node),
                                      Left    => Left_Neighbor(Node),
                                      Right   => Right_Neighbor(Node));
      Key_Read_Context    : Key_Context_Type   := New_Key_Context;
      Key_Write_Context   : Key_Context_Type   := New_Key_Context;
      Value_Read_Context  : Value_Context_Type := New_Value_Context;
      Value_Write_Context : Value_Context_Type := New_Value_Context;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                    Value_Read_Context, Value_Write_Context);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, Value_Write_Context,
                       IO.Blocks.Base_Block_Type(N), Index, Key, Value);
      if not Is_Ok(N) then
         return N;
      end if;

      for I in Index .. Degree(Node) loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                    Value_Read_Context, Value_Write_Context, +1);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;
      return N;
   end Insertion;


   function Substitution
     (Node  : RW_Node_Type;
      Index : Valid_Index_Type;
      Key   : Key_Type;
      Child : Valid_Address_Type)
      return RW_Node_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Inner(Node));
      pragma Assert (Is_Valid(Child));

      N : RW_Node_Type := New_RW_Node(Is_Ok   => True,
                                      Is_Free => Is_Free(Node),
                                      Is_Leaf => Is_Leaf(Node),
                                      Degree  => Degree(Node),
                                      Parent  => Parent(Node),
                                      Left    => Left_Neighbor(Node),
                                      Right   => Right_Neighbor(Node));
      Key_Read_Context  : Key_Context_Type := New_Key_Context;
      Key_Write_Context : Key_Context_Type := New_Key_Context;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, IO.Blocks.Base_Block_Type(N), Index,
                       Key, Child);
      if not Is_Ok(N) then
         return N;
      end if;

      for I in Index + 1 .. Degree(Node) loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;
      return N;
   end Substitution;


   function Substitution
     (Node  : RW_Node_Type;
      Index : Valid_Index_Type;
      Key   : Key_Type;
      Value : Value_Type)
      return RW_Node_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Leaf(Node));

      N : RW_Node_Type := New_RW_Node(Is_Ok   => True,
                                      Is_Free => Is_Free(Node),
                                      Is_Leaf => Is_Leaf(Node),
                                      Degree  => Degree(Node),
                                      Parent  => Parent(Node),
                                      Left    => Left_Neighbor(Node),
                                      Right   => Right_Neighbor(Node));
      Key_Read_Context    : Key_Context_Type   := New_Key_Context;
      Key_Write_Context   : Key_Context_Type   := New_Key_Context;
      Value_Read_Context  : Value_Context_Type := New_Value_Context;
      Value_Write_Context : Value_Context_Type := New_Value_Context;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                    Value_Read_Context, Value_Write_Context);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, Value_Write_Context,
                       IO.Blocks.Base_Block_Type(N), Index, Key, Value);
      if not Is_Ok(N) then
         return N;
      end if;

      for I in Index + 1 .. Degree(Node) loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                    Value_Read_Context, Value_Write_Context);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;
      return N;
   end Substitution;


   function Deletion
     (Node  : RW_Node_Type;
      Index : Valid_Index_Type)
      return RW_Node_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (not Is_Free(Node));

      N : RW_Node_Type := New_RW_Node(Is_Ok   => True,
                                      Is_Free => Is_Free(Node),
                                      Is_Leaf => Is_Leaf(Node),
                                      Degree  => Degree(Node) - 1,
                                      Parent  => Parent(Node),
                                      Left    => Left_Neighbor(Node),
                                      Right   => Right_Neighbor(Node));
   begin
      if Is_Inner(Node) then
         declare
            Key_Read_Context  : Key_Context_Type := New_Key_Context;
            Key_Write_Context : Key_Context_Type := New_Key_Context;
         begin
            for I in 1 .. Index - 1 loop
               Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context);
            end loop;
            for I in Index + 1 .. Degree(Node) loop
               Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context, -1);
            end loop;
         end;
      else
         declare
            Key_Read_Context    : Key_Context_Type   := New_Key_Context;
            Key_Write_Context   : Key_Context_Type   := New_Key_Context;
            Value_Read_Context  : Value_Context_Type := New_Value_Context;
            Value_Write_Context : Value_Context_Type := New_Value_Context;
         begin
            for I in 1 .. Index - 1 loop
               Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                          Value_Read_Context, Value_Write_Context);
               if not Is_Ok(N) then
                  return N;
               end if;
            end loop;
            for I in Index + 1 .. Degree(Node) loop
               Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                          Value_Read_Context, Value_Write_Context, -1);
               if not Is_Ok(N) then
                  return N;
               end if;
            end loop;
         end;
      end if;
      return N;
   end Deletion;


   function Copy
     (Node : RW_Node_Type;
      From : Valid_Index_Type;
      To   : Index_Type)
      return RW_Node_Type is
   begin
      if not Nodes.Is_Ok(Node) then
         return Invalid_Node;
      end if;

      declare
         pragma Assert (not Is_Free(Node));
         N : RW_Node_Type := New_RW_Node(Is_Ok   => True,
                                         Is_Free => Is_Free(Node),
                                         Is_Leaf => Is_Leaf(Node),
                                         Degree  => To - From + 1,
                                         Parent  => Parent(Node),
                                         Left    => Left_Neighbor(Node),
                                         Right   => Right_Neighbor(Node));
         Shift_By : constant Integer := -1 * Integer(From) + 1;
      begin
         if Is_Inner(Node) then
            declare
               Key_Read_Context  : Key_Context_Type := New_Key_Context;
               Key_Write_Context : Key_Context_Type := New_Key_Context;
            begin
               for I in From .. To loop
                  Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                             Shift_By);
                  if not Is_Ok(N) then
                     return N;
                  end if;
               end loop;
            end;
         else
            declare
               Key_Read_Context    : Key_Context_Type   := New_Key_Context;
               Key_Write_Context   : Key_Context_Type   := New_Key_Context;
               Value_Read_Context  : Value_Context_Type := New_Value_Context;
               Value_Write_Context : Value_Context_Type := New_Value_Context;
            begin
               for I in From .. To loop
                  Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                             Value_Read_Context, Value_Write_Context, Shift_By);
                  if not Is_Ok(N) then
                     return N;
                  end if;
               end loop;
            end;
         end if;
         return N;
      end;
   end Copy;


   function Combi_Copy
     (Left_Node  : RW_Node_Type;
      Right_Node : RW_Node_Type;
      From       : Valid_Index_Type;
      To         : Index_Type)
      return RW_Node_Type is
   begin
      if not Is_Ok(Left_Node) or not Is_Ok(Right_Node) then
         return Invalid_Node;
      end if;

      declare
         pragma Assert (not Is_Free(Left_Node));
         pragma Assert (not Is_Free(Right_Node));

         function Min is new Utils.Gen_Minimum(Index_Type);
         pragma Inline (Min);
         function Max is new Utils.Gen_Maximum(Index_Type);
         pragma Inline (Max);

         function Combi_Parent
           (Left_Node  : RW_Node_Type;
            Right_Node : RW_Node_Type;
            To         : Index_Type)
            return Address_Type is
         begin
            if To <= Degree(Left_Node) then
               return Parent(Left_Node);
            else
               return Parent(Right_Node);
            end if;
         end Combi_Parent;

         function Combi_Left_Neighbor
           (Left_Node  : RW_Node_Type;
            Right_Node : RW_Node_Type;
            From       : Index_Type)
            return Address_Type is
         begin
            if From <= Degree(Left_Node) then
               return Left_Neighbor(Left_Node);
            else
               return Left_Neighbor(Right_Node);
            end if;
         end Combi_Left_Neighbor;

         function Combi_Right_Neighbor
           (Left_Node  : RW_Node_Type;
            Right_Node : RW_Node_Type;
            To         : Index_Type)
            return Address_Type is
         begin
            if To <= Degree(Left_Node) then
               return Right_Neighbor(Left_Node);
            else
               return Right_Neighbor(Right_Node);
            end if;
         end Combi_Right_Neighbor;

         N : RW_Node_Type := New_RW_Node
                 (Is_Ok   => True,
                  Is_Free => Is_Free(Left_Node),
                  Is_Leaf => Is_Leaf(Left_Node),
                  Degree  => To - From + 1,
                  Parent  => Combi_Parent(Left_Node, Right_Node, To),
                  Left    => Combi_Left_Neighbor(Left_Node, Right_Node, From),
                  Right   => Combi_Right_Neighbor(Left_Node, Right_Node, To));
         Left_Shift_By  : constant Integer
                        := -1 * Integer(From) + 1;
         Right_Shift_By : constant Integer
                        := -1 * Integer(From) + 1 + Integer(Degree(Left_Node));
      begin
         if Is_Inner(Left_Node) then
            declare
               Key_Read_Context  : Key_Context_Type := New_Key_Context;
               Key_Write_Context : Key_Context_Type := New_Key_Context;
            begin
               for I in From .. Min(To, Degree(Left_Node)) loop
                  Copy_Entry(N, Left_Node, I, Key_Read_Context,
                             Key_Write_Context, Left_Shift_By);
                  if not Is_Ok(N) then
                     return N;
                  end if;
               end loop;
               for I in Max(From, Degree(Left_Node) + 1) .. To loop
                  Copy_Entry(N, Right_Node, I - Degree(Left_Node),
                             Key_Read_Context, Key_Write_Context,
                             Right_Shift_By);
                  if not Is_Ok(N) then
                     return N;
                  end if;
               end loop;
            end;
         else
            declare
               Key_Read_Context    : Key_Context_Type   := New_Key_Context;
               Key_Write_Context   : Key_Context_Type   := New_Key_Context;
               Value_Read_Context  : Value_Context_Type := New_Value_Context;
               Value_Write_Context : Value_Context_Type := New_Value_Context;
            begin
               for I in From .. Min(To, Degree(Left_Node)) loop
                  Copy_Entry(N, Left_Node, I, Key_Read_Context,
                             Key_Write_Context, Value_Read_Context,
                             Value_Write_Context, Left_Shift_By);
                  if not Is_Ok(N) then
                     return N;
                  end if;
               end loop;
               for I in Max(From, Degree(Left_Node) + 1) .. To loop
                  Copy_Entry(N, Right_Node, I - Degree(Left_Node),
                             Key_Read_Context, Key_Write_Context,
                             Value_Read_Context, Value_Write_Context,
                             Right_Shift_By);
                  if not Is_Ok(N) then
                     return N;
                  end if;
               end loop;
            end;
         end if;
         return N;
      end;
   end Combi_Copy;


   function Combination
     (Left_Node  : RW_Node_Type;
      Right_Node : RW_Node_Type)
      return RW_Node_Type is
   begin
      if not Is_Ok(Left_Node) or not Is_Ok(Right_Node) then
         return Invalid_Node;
      end if;

      declare
         pragma Assert (Is_Ok(Left_Node));
         pragma Assert (Is_Ok(Right_Node));
         pragma Assert (Validation(Left_Node) /= Too_Large or
                        Validation(Right_Node) /= Too_Large);
         pragma Assert (not Is_Free(Left_Node));
         pragma Assert (not Is_Free(Right_Node));
         pragma Assert (Is_Leaf(Left_Node) = Is_Leaf(Right_Node));

         Left_Degree  : constant Degree_Type := Degree(Left_Node);
         Right_Degree : constant Degree_Type := Degree(Right_Node);
         Degree       : constant Degree_Type := Left_Degree + Right_Degree;
         N : RW_Node_Type
           := New_RW_Node(Is_Ok   => True,
                          Is_Free => Is_Free(Right_Node),
                          Is_Leaf => Is_Leaf(Right_Node),
                          Degree  => Degree,
                          Parent  => Parent(Right_Node),
                          Left    => Left_Neighbor(Left_Node),
                          Right   => Right_Neighbor(Right_Node));
      begin
         if Is_Inner(Right_Node) then
            declare
               Key_Read_Context  : Key_Context_Type := New_Key_Context;
               Key_Write_Context : Key_Context_Type := New_Key_Context;
            begin
               for I in 1 .. Left_Degree loop
                  Copy_Entry(N, Left_Node, I, Key_Read_Context,
                             Key_Write_Context);
                  if not Is_Ok(N) then
                     return N;
                  end if;
               end loop;
               for I in 1 .. Right_Degree loop
                  Copy_Entry(N, Right_Node, I, Key_Read_Context,
                             Key_Write_Context, +1 * Integer(Left_Degree));
                  if not Is_Ok(N) then
                     return N;
                  end if;
               end loop;
            end;
         else
            declare
               Key_Read_Context    : Key_Context_Type   := New_Key_Context;
               Key_Write_Context   : Key_Context_Type   := New_Key_Context;
               Value_Read_Context  : Value_Context_Type := New_Value_Context;
               Value_Write_Context : Value_Context_Type := New_Value_Context;
            begin
               for I in 1 .. Left_Degree loop
                  Copy_Entry(N, Left_Node, I, Key_Read_Context,
                             Key_Write_Context, Value_Read_Context,
                             Value_Write_Context);
                  if not Is_Ok(N) then
                     return N;
                  end if;
               end loop;
               for I in 1 .. Right_Degree loop
                  Copy_Entry(N, Right_Node, I, Key_Read_Context,
                             Key_Write_Context, Value_Read_Context,
                             Value_Write_Context, +1 * Integer(Left_Degree));
                  if not Is_Ok(N) then
                     return N;
                  end if;
               end loop;
            end;
         end if;
         return N;
      end;
   end Combination;


   ----------
   -- Block-related subprograms.

   function Size_Of
     (Node : Node_Type)
      return IO.Blocks.Size_Type
   is
      pragma Assert (Nodes.Is_Ok(Node));
      Last_Used : constant IO.Blocks.Base_Position_Type
                := Phys.Total_Size(IO.Blocks.Base_Block_Type(Node));
   begin
      return IO.Blocks.Size_Type(Last_Used);
   end Size_Of;


   function Is_Valid
     (Node           : Node_Type;
      Force_Non_Root : Boolean := False)
      return Boolean is
   begin
      return Validation(Node, Force_Non_Root) = Valid;
   end Is_Valid;


   function Validation
     (Node           : Node_Type;
      Force_Non_Root : Boolean := False)
      return Validation_State_Type
   is
      use type IO.Blocks.Size_Type;
   begin
      if not Nodes.Is_Ok(Node) then
         return Too_Large;
      end if;

      if Is_Free(Node) then
         return Valid;
      end if;

      declare
         use type IO.Blocks.Base_Position_Type;
         Min_Degree : Degree_Type;
         Min_Space  : IO.Blocks.Position_Type;
         Max_Space  : constant IO.Blocks.Position_Type
                    := Phys.Effective_Block_Space;
         Needed     : constant IO.Blocks.Base_Position_Type
                    := Phys.Entries_Size(IO.Blocks.Base_Block_Type(Node));
      begin
         if Is_Root(Node) and not Force_Non_Root then
            Min_Degree := 0;
            Min_Space  := 0;
         elsif not Is_Context_Free_Serialization then
            Min_Degree := 2;
            Min_Space  := 0;
         else
            Min_Degree := 2;
            Min_Space  := Max_Space * 3 / 8;
         end if;

         if Needed < Min_Space or Degree(Node) < Min_Degree then
            return Too_Small;
         elsif Needed > Max_Space then
            return Too_Large;
         else
            return Valid;
         end if;
      end;
   end Validation;


   function Max_Key_Size
     (Max_Value_Size : IO.Blocks.Size_Type)
      return IO.Blocks.Size_Type
   is
      use type IO.Blocks.Base_Position_Type;
      Max_Entry_Size        : constant IO.Blocks.Base_Position_Type
                            := Phys.Effective_Block_Space * 1 / 4;
      Max_Value_Size_As_Pos : constant IO.Blocks.Position_Type
                            := IO.Blocks.Base_Position_Type(Max_Value_Size);
   begin
      return IO.Blocks.Size_Type
               (Phys.Max_Key_Size(Max_Entry_Size => Max_Entry_Size,
                                  Max_Value_Size => Max_Value_Size_As_Pos));
   end Max_Key_Size;


   function To_Block
     (Node : Node_Type)
      return IO.Blocks.Block_Type
   is
      pragma Assert (Nodes.Is_Ok(Node));
      use type IO.Blocks.Base_Position_Type;
   begin
      pragma Assert (Phys.Total_Size(IO.Blocks.Base_Block_Type(Node)) <=
                     IO.Blocks.Block_Size);
      pragma Assert (Validation(Node) = Valid);
      return IO.Blocks.To_Block
               (Block     => IO.Blocks.Base_Block_Type(Node),
                Last_Used => Phys.Total_Size(IO.Blocks.Base_Block_Type(Node)));
   end To_Block;

end Nodes;

