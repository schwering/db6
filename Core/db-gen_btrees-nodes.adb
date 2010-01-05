-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils.Binary_Search;
with DB.Utils.Gen_Minimum;
with DB.Utils.Gen_Maximum;
with DB.Utils.Print;

separate (DB.Gen_BTrees)
package body Nodes is

   -- The layout of a Block, i.e. a byte sequence is the following:
   -- 1. Meta_Data
   -- If Degree(N) > 0:
   -- 2. If Is_Leaf(N): Degree(N) times Index_Type (end position of entry)
   --                   where an entry is either (Key, Child) or (Key, Value)
   -- 3. If Is_Leaf(N): Key1, Value1, ..., KeyDegree(N), ValueDegree(N)
   --    Else:          Key1, Child_Type, ..., KeyDegree(N), Child_Type
   package Phys is
      pragma Elaborate_Body;

      type Child_Type is
         record
            Address : Valid_Address_Type;
            Count   : Count_Type;
         end record;
      pragma Pack (Child_Type);

      function Entry_Size
        (Block : IO.Blocks.Long_Block_Type;
         Index : Valid_Index_Type)
         return IO.Blocks.Long_Position_Type;

      function Entries_Size
        (Block : IO.Blocks.Long_Block_Type)
         return IO.Blocks.Long_Position_Type;

      function Total_Size
        (Block : IO.Blocks.Long_Block_Type)
         return IO.Blocks.Long_Position_Type;

      function Effective_Block_Space
        return IO.Blocks.Long_Position_Type;

      function Max_Key_Size
        (Max_Entry_Size : IO.Blocks.Long_Position_Type;
         Max_Value_Size : IO.Blocks.Long_Position_Type)
         return IO.Blocks.Long_Position_Type;

      function Last_Used_Index
        (Block : IO.Blocks.Long_Block_Type)
         return IO.Blocks.Long_Index_Type;

      function New_Block
        (Is_Free : Boolean;
         Degree  : Degree_Type;
         Is_Leaf : Boolean;
         Parent  : Address_Type;
         Left    : Address_Type;
         Right   : Address_Type)
         return IO.Blocks.Long_Block_Type;

      function Is_Free
        (Block : IO.Blocks.Long_Block_Type)
         return Boolean;

      function Degree
        (Block : IO.Blocks.Long_Block_Type)
         return Degree_Type;

      function Is_Leaf
        (Block : IO.Blocks.Long_Block_Type)
         return Boolean;

      function Parent
        (Block : IO.Blocks.Long_Block_Type)
         return Address_Type;

      procedure Set_Parent
        (Block   : in out IO.Blocks.Long_Block_Type;
         Address : in     Address_Type);

      function Left
        (Block : IO.Blocks.Long_Block_Type)
         return Address_Type;

      procedure Set_Left
        (Block   : in out IO.Blocks.Long_Block_Type;
         Address : in     Address_Type);

      function Right
        (Block : IO.Blocks.Long_Block_Type)
         return Address_Type;

      procedure Set_Right
        (Block   : in out IO.Blocks.Long_Block_Type;
         Address : in     Address_Type);

      procedure Read_Key
        (Key_Context : in out Key_Context_Type;
         Block       : in     IO.Blocks.Long_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Key_Type;
         Success     :    out Boolean);

      procedure Read_Child
        (Key_Context : in out Key_Context_Type;
         Block       : in     IO.Blocks.Long_Block_Type;
         Index       : in     Valid_Index_Type;
         Child       :    out Child_Type;
         Success     :    out Boolean);

      procedure Read_Value
        (Key_Context   : in out Key_Context_Type;
         Value_Context : in out Value_Context_Type;
         Block         : in     IO.Blocks.Long_Block_Type;
         Index         : in     Valid_Index_Type;
         Value         :    out Value_Type;
         Success       :    out Boolean);

      procedure Read_Entry
        (Key_Context : in out Key_Context_Type;
         Block       : in     IO.Blocks.Long_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Key_Type;
         Child       :    out Child_Type;
         Success     :    out Boolean);

      procedure Read_Entry
        (Key_Context   : in out Key_Context_Type;
         Value_Context : in out Value_Context_Type;
         Block         : in     IO.Blocks.Long_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           :    out Key_Type;
         Value         :    out Value_Type;
         Success       :    out Boolean);

      procedure Write_Entry
        (Key_Context : in out Key_Context_Type;
         Block       : in out IO.Blocks.Long_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         : in     Key_Type;
         Child       : in     Child_Type;
         Success     :    out Boolean);

      procedure Write_Entry
        (Key_Context   : in out Key_Context_Type;
         Value_Context : in out Value_Context_Type;
         Block         : in out IO.Blocks.Long_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           : in     Key_Type;
         Value         : in     Value_Type;
         Success       :    out Boolean);

      procedure Write_Child
        (Key_Context : in out Key_Context_Type;
         Block       : in out IO.Blocks.Long_Block_Type;
         Index       : in     Valid_Index_Type;
         Child       : in     Child_Type;
         Success     :    out Boolean);

      private
         pragma Inline (Entry_Size);
         pragma Inline (Entries_Size);
         pragma Inline (Total_Size);
         pragma Inline (Effective_Block_Space);
         pragma Inline (Last_Used_Index);
         pragma Inline (New_Block);
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
      use type IO.Blocks.Long_Position_Type;

      function Size_Of is new IO.Blocks.Size_Of(Degree_Type);
      function Size_Of is new IO.Blocks.Size_Of(Boolean);
      function Size_Of is new IO.Blocks.Size_Of(Address_Type);
      function Size_Of is new IO.Blocks.Size_Of(IO.Blocks.Position_Type);
      function Size_Of is new IO.Blocks.Size_Of(Child_Type);

      Size_Of_Degree    : constant IO.Blocks.Long_Position_Type :=
         IO.Blocks.Long_Position_Type(Size_Of(Degree_Type'(0)));

      Size_Of_Boolean   : constant IO.Blocks.Long_Position_Type :=
         IO.Blocks.Long_Position_Type(Size_Of(Boolean'(True)));

      Size_Of_Address   : constant IO.Blocks.Long_Position_Type :=
         IO.Blocks.Long_Position_Type(Size_Of(Invalid_Address));

      Size_Of_Position  : constant  IO.Blocks.Long_Position_Type :=
         IO.Blocks.Long_Position_Type
            (Size_Of(IO.Blocks.Position_Type'Last));

      Size_Of_Child     : constant IO.Blocks.Long_Position_Type :=
         IO.Blocks.Long_Position_Type(Size_Of(Child_Type'(others => <>)));

      Size_Of_Meta_Data : constant IO.Blocks.Long_Position_Type :=
         Size_Of_Boolean + Size_Of_Degree +
         Size_Of_Boolean + Size_Of_Address +
         Size_Of_Address + Size_Of_Address;

      -- Layout of Node Blocks is as follows:
      -- 1. Is_Free (Size_Of_Boolean)
      -- 2. Degree  (Size_Of_Degree)
      -- 3. Is_Leaf (Size_Of_Boolean)
      -- 4. Parent  (Size_Of_Address)
      -- 5. Left    (Size_Of_Address)
      -- 6. Right   (Size_Of_Address)
      -- 7. Indexes (|Degree| * Size_Of_Position)
      -- 8. Entries (Size_Of(Key_1) + Size_Of_Child ..
      --             Size_Of(Key_N) + Size_Of_Child)
      --         or (Size_Of(Key_1) + Size_Of(Value_1) ..
      --             Size_Of(Key_N) + Size_Of(Value_N))


      function "*"
        (I : Valid_Index_Type;
         J : IO.Blocks.Long_Index_Type)
         return IO.Blocks.Long_Index_Type
      is
         pragma Inline ("*");
      begin
         return IO.Blocks.Long_Index_Type(I) * J;
      end "*";


      function Pos_From_Pos
        (Entry_Index : Valid_Index_Type)
         return IO.Blocks.Long_Index_Type
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
         return IO.Blocks.Long_Index_Type
      is
         pragma Inline (Pos_To_Pos);
      begin
         return Size_Of_Meta_Data + Entry_Index * Size_Of_Position;
      end Pos_To_Pos;


      function Entry_To_Pos
        (Block       : IO.Blocks.Long_Block_Type;
         Entry_Index : Valid_Index_Type)
         return IO.Blocks.Long_Index_Type
      is
         pragma Inline (Entry_To_Pos);
         pragma Assert (not Is_Free(Block));
         pragma Assert (Entry_Index in 1 .. Degree(Block));
         procedure Read is new IO.Blocks.Read_At(IO.Blocks.Long_Index_Type);
         Index : IO.Blocks.Long_Index_Type;
      begin
         Read(Block, Pos_From_Pos(Entry_Index), Pos_To_Pos(Entry_Index),
              Index);
         pragma Assert (Index'Valid);
         return Index;
      end Entry_To_Pos;


      function Entry_From_Pos
        (Block       : IO.Blocks.Long_Block_Type;
         Entry_Index : Valid_Index_Type)
         return IO.Blocks.Long_Position_Type
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
        (Block : IO.Blocks.Long_Block_Type;
         Index : Valid_Index_Type)
         return IO.Blocks.Cursor_Type
      is
         pragma Inline (New_Cursor_From);
      begin
         return IO.Blocks.New_Cursor(Entry_From_Pos(Block, Index));
      end New_Cursor_From;


      procedure Set_Entry_Size
        (Block         : in out IO.Blocks.Long_Block_Type;
         Entry_Index   : in     Valid_Index_Type;
         Raw_Data_Size : in     IO.Blocks.Long_Position_Type)
      is
         pragma Inline (Set_Entry_Size);
         pragma Assert (not Is_Free(Block));
         pragma Assert (Entry_Index in 1 .. Degree(Block));
         procedure Write is new IO.Blocks.Write_At(IO.Blocks.Long_Index_Type);
         From : constant IO.Blocks.Long_Index_Type
              := Entry_From_Pos(Block, Entry_Index);
         To   : constant IO.Blocks.Long_Index_Type
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
        (Block : IO.Blocks.Long_Block_Type;
         Index : Valid_Index_Type)
         return IO.Blocks.Long_Position_Type is
      begin
         return Entry_To_Pos(Block, Index) - Entry_From_Pos(Block, Index) + 1 +
                Size_Of_Position;
      end Entry_Size;


      function Entries_Size
        (Block : IO.Blocks.Long_Block_Type)
         return IO.Blocks.Long_Position_Type is
      begin
         if Degree(Block) = 0 then
            return 0;
         else
            return Entry_To_Pos(Block, Degree(Block)) -
                   Size_Of_Meta_Data;
         end if;
      end Entries_Size;


      function Total_Size
        (Block : IO.Blocks.Long_Block_Type)
         return IO.Blocks.Long_Position_Type is
      begin
         if Is_Free(Block) then
            return Size_Of_Meta_Data;
         else
            return Size_Of_Meta_Data + Entries_Size(Block);
         end if;
      end Total_Size;


      function Effective_Block_Space
        return IO.Blocks.Long_Position_Type is
      begin
         return IO.Blocks.Index_Type'Last - Size_Of_Meta_Data;
      end Effective_Block_Space;


      function Max_Key_Size
        (Max_Entry_Size : IO.Blocks.Long_Position_Type;
         Max_Value_Size : IO.Blocks.Long_Position_Type)
         return IO.Blocks.Long_Position_Type is
      begin
         if Size_Of_Child > Max_Value_Size then
            return Max_Entry_Size - Size_Of_Child - Size_Of_Position;
         else
            return Max_Entry_Size - Max_Value_Size - Size_Of_Position;
         end if;
      end Max_Key_Size;


      function Last_Used_Index
        (Block : IO.Blocks.Long_Block_Type)
         return IO.Blocks.Long_Index_Type is
      begin
         if Degree(Block) = 0 then
            return IO.Blocks.Long_Index_Type(Size_Of_Meta_Data);
         else
            return Entry_To_Pos(Block, Degree(Block));
         end if;
      end Last_Used_Index;


      function Free_Space
        (Block : IO.Blocks.Long_Block_Type)
         return IO.Blocks.Long_Position_Type
      is
         pragma Inline (Free_Space);
      begin
         return IO.Blocks.Long_Index_Type'Last - Last_Used_Index(Block);
      end Free_Space;
      pragma Unreferenced (Free_Space);


      function Is_Free
        (Block : IO.Blocks.Long_Block_Type)
         return Boolean
      is
         procedure Read is new IO.Blocks.Read_At(Boolean);
         Offset : constant IO.Blocks.Long_Position_Type := 0;
         From   : constant IO.Blocks.Long_Index_Type
                := IO.Blocks.Long_Index_Type'First + Offset;
         Free   : Boolean;
      begin
         Read(Block, From, From + Size_Of_Boolean - 1, Free);
         return Free;
      end Is_Free;


      procedure Set_Free
        (Block : in out IO.Blocks.Long_Block_Type;
         Free  : in     Boolean)
      is
         procedure Write is new IO.Blocks.Write_At(Boolean);
         Offset : constant IO.Blocks.Long_Position_Type := 0;
         From   : constant IO.Blocks.Long_Index_Type
                := IO.Blocks.Long_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Boolean - 1, Free);
         pragma Assert (Is_Free(Block) = Free);
      end Set_Free;


      function Degree
        (Block : IO.Blocks.Long_Block_Type)
         return Degree_Type
      is
         pragma Assert (not Is_Free(Block));
         procedure Read is new IO.Blocks.Read_At(Degree_Type);
         Offset : constant IO.Blocks.Long_Position_Type := Size_Of_Boolean;
         From   : constant IO.Blocks.Long_Index_Type
                := IO.Blocks.Long_Index_Type'First + Offset;
         Degree : Degree_Type;
      begin
         Read(Block, From, From + Size_Of_Degree - 1, Degree);
         pragma Assert (Degree'Valid);
         return Degree;
      end Degree;


      procedure Set_Degree
        (Block  : in out IO.Blocks.Long_Block_Type;
         Degree : in     Degree_Type)
      is
         pragma Assert (not Is_Free(Block));
         procedure Write is new IO.Blocks.Write_At(Degree_Type);
         Offset : constant IO.Blocks.Long_Position_Type := Size_Of_Boolean;
         From   : constant IO.Blocks.Long_Index_Type
                := IO.Blocks.Long_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Degree - 1, Degree);
         pragma Assert (Degree = Phys.Degree(Block));
      end Set_Degree;


      function Is_Leaf
        (Block : IO.Blocks.Long_Block_Type)
        return Boolean
      is
         pragma Assert (not Is_Free(Block));
         procedure Read is new IO.Blocks.Read_At(Boolean);
         Offset : constant IO.Blocks.Long_Position_Type
                := Size_Of_Boolean + Size_Of_Degree;
         From   : constant IO.Blocks.Long_Index_Type
                := IO.Blocks.Long_Index_Type'First + Offset;
         Leaf   : Boolean;
      begin
         Read(Block, From, From + Size_Of_Boolean - 1, Leaf);
         return Leaf;
      end Is_Leaf;


      procedure Set_Leaf
        (Block : in out IO.Blocks.Long_Block_Type;
         Leaf  : in Boolean)
      is
         pragma Assert (not Is_Free(Block));
         procedure Write is new IO.Blocks.Write_At(Boolean);
         Offset : constant IO.Blocks.Long_Position_Type
                := Size_Of_Boolean + Size_Of_Degree;
         From   : constant IO.Blocks.Long_Index_Type
                := IO.Blocks.Long_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Boolean - 1, Leaf);
         pragma Assert (Is_Leaf(Block) = Leaf);
      end Set_Leaf;


      function Parent
        (Block : IO.Blocks.Long_Block_Type)
         return Address_Type
      is
         pragma Assert (not Is_Free(Block));
         procedure Read is new IO.Blocks.Read_At(Address_Type);
         Offset  : constant IO.Blocks.Long_Position_Type
                 := Size_Of_Boolean + Size_Of_Degree + Size_Of_Boolean;
         From    : constant IO.Blocks.Long_Index_Type
                 := IO.Blocks.Long_Index_Type'First + Offset;
         Address : Address_Type;
      begin
         Read(Block, From, From + Size_Of_Address - 1, Address);
         return Address;
      end Parent;


      procedure Set_Parent
        (Block   : in out IO.Blocks.Long_Block_Type;
         Address : in     Address_Type)
      is
         pragma Assert (not Is_Free(Block));
         procedure Write is new IO.Blocks.Write_At(Address_Type);
         Offset : constant IO.Blocks.Long_Position_Type
                := Size_Of_Boolean + Size_Of_Degree + Size_Of_Boolean;
         From   : constant IO.Blocks.Long_Index_Type
                := IO.Blocks.Long_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Address - 1, Address);
         pragma Assert (Parent(Block) = Address);
      end Set_Parent;


      function Left
        (Block : IO.Blocks.Long_Block_Type)
         return Address_Type
      is
         procedure Read is new IO.Blocks.Read_At(Address_Type);
         Offset  : constant IO.Blocks.Long_Position_Type
                 := Size_Of_Boolean + Size_Of_Degree + Size_Of_Boolean +
                    Size_Of_Address;
         From    : constant IO.Blocks.Long_Index_Type
                 := IO.Blocks.Long_Index_Type'First + Offset;
         Address : Address_Type;
      begin
         Read(Block, From, From + Size_Of_Address - 1, Address);
         return Address;
      end Left;


      procedure Set_Left
        (Block   : in out IO.Blocks.Long_Block_Type;
         Address : in     Address_Type)
      is
         procedure Write is new IO.Blocks.Write_At(Address_Type);
         Offset : constant IO.Blocks.Long_Position_Type
                := Size_Of_Boolean + Size_Of_Degree + Size_Of_Boolean +
                   Size_Of_Address;
         From   : constant IO.Blocks.Long_Index_Type
                := IO.Blocks.Long_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Address - 1, Address);
         pragma Assert (Left(Block) = Address);
      end Set_Left;


      function Right
        (Block : IO.Blocks.Long_Block_Type)
         return Address_Type
      is
         procedure Read is new IO.Blocks.Read_At(Address_Type);
         Offset  : constant IO.Blocks.Long_Position_Type
                 := Size_Of_Boolean + Size_Of_Degree + Size_Of_Boolean +
                    Size_Of_Address + Size_Of_Address;
         From    : constant IO.Blocks.Long_Index_Type
                 := IO.Blocks.Long_Index_Type'First + Offset;
         Address : Address_Type;
      begin
         Read(Block, From, From + Size_Of_Address - 1, Address);
         return Address;
      end Right;


      procedure Set_Right
        (Block   : in out IO.Blocks.Long_Block_Type;
         Address : in     Address_Type)
      is
         procedure Write is new IO.Blocks.Write_At(Address_Type);
         Offset : constant IO.Blocks.Long_Position_Type
                := Size_Of_Boolean + Size_Of_Degree + Size_Of_Boolean +
                   Size_Of_Address + Size_Of_Address;
         From   : constant IO.Blocks.Long_Index_Type
                := IO.Blocks.Long_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Address - 1, Address);
         pragma Assert (Right(Block) = Address);
      end Set_Right;


      function New_Block
        (Is_Free : Boolean;
         Degree  : Degree_Type;
         Is_Leaf : Boolean;
         Parent  : Address_Type;
         Left    : Address_Type;
         Right   : Address_Type)
         return IO.Blocks.Long_Block_Type
      is
         Block : IO.Blocks.Long_Block_Type;
      begin
         Set_Free(Block, Is_Free);
         if not Is_Free then
            Set_Degree(Block, Degree);
            Set_Leaf(Block, Is_Leaf);
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
         return Block;
      end New_Block;


      procedure Read_Key
        (Key_Context : in out Key_Context_Type;
         Block       : in     IO.Blocks.Long_Block_Type;
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
         Block       : in     IO.Blocks.Long_Block_Type;
         Index       : in     Valid_Index_Type;
         Child       :    out Child_Type;
         Success     :    out Boolean)
      is
         pragma Assert (not Is_Leaf(Block));
         procedure Read_Child is new IO.Blocks.Read(Child_Type);
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
         Block         : in     IO.Blocks.Long_Block_Type;
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
         Block       : in     IO.Blocks.Long_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Key_Type;
         Child       :    out Child_Type;
         Success     :    out Boolean)
      is
         pragma Assert (not Is_Leaf(Block));
         procedure Read_Child is new IO.Blocks.Read(Child_Type);
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
         Block         : in     IO.Blocks.Long_Block_Type;
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
         Block       : in out IO.Blocks.Long_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         : in     Key_Type;
         Child       : in     Child_Type;
         Success     :    out Boolean)
      is
         pragma Assert (not Is_Leaf(Block));
         procedure Write_Child is new IO.Blocks.Write(Child_Type);
         Cursor : IO.Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Write_Key(Key_Context, Block, Cursor, Key);
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Write_Child(Block, Cursor, Child);
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         declare
            Size : constant IO.Blocks.Long_Position_Type
                 := IO.Blocks.Position(Cursor) - Entry_From_Pos(Block, Index);
         begin
            Set_Entry_Size(Block, Index, Size);
         end;
      end Write_Entry;


      procedure Write_Entry
        (Key_Context   : in out Key_Context_Type;
         Value_Context : in out Value_Context_Type;
         Block         : in out IO.Blocks.Long_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           : in     Key_Type;
         Value         : in     Value_Type;
         Success       :    out Boolean)
      is
         pragma Assert (Is_Leaf(Block));
         Cursor : IO.Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Write_Key(Key_Context, Block, Cursor, Key);
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Write_Value(Value_Context, Block, Cursor, Value);
         Success := IO.Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         declare
            Size : constant IO.Blocks.Long_Position_Type
                 := IO.Blocks.Position(Cursor) - Entry_From_Pos(Block, Index);
         begin
            Set_Entry_Size(Block, Index, Size);
         end;

         --declare
         --   Key_Context : Key_Context_Type;
         --   Value_Context : Value_Context_Type;
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
         Block       : in out IO.Blocks.Long_Block_Type;
         Index       : in     Valid_Index_Type;
         Child       : in     Child_Type;
         Success     :    out Boolean)
      is
         pragma Assert (not Is_Leaf(Block));
         procedure Write_Child is new IO.Blocks.Write(Child_Type);
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
         Write_Child(Block, Cursor, Child);
         Success := IO.Blocks.Is_Valid(Cursor);
      end Write_Child;

    end Phys;


   function Root_Node
     (Is_Leaf : Boolean)
      return Node_Type is
   begin
      return Node_Type'(Ok    => True,
                        Block => Phys.New_Block(Is_Free => False,
                                                Degree  => 0,
                                                Is_Leaf => Is_Leaf,
                                                Parent  => Invalid_Address,
                                                Left    => Invalid_Address,
                                                Right   => Invalid_Address));
   end Root_Node;


   function Free_Node
      return Node_Type
   is
   begin
      return Node_Type'(Ok    => True,
                        Block => Phys.New_Block(Is_Free => True,
                                                Degree  => 0,
                                                Is_Leaf => False,
                                                Parent  => Invalid_Address,
                                                Left    => Invalid_Address,
                                                Right   => Invalid_Address));
   end Free_Node;


   function Is_Free
     (Node : Node_Type)
      return Boolean
   is
      pragma Assert (Node.Ok);
   begin
      return Phys.Is_Free(Node.Block);
   end Is_Free;


   function Degree
     (Node : Node_Type)
      return Degree_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      return Phys.Degree(Node.Block);
   end Degree;


   function Is_Leaf
     (Node : Node_Type)
      return Boolean
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      return Phys.Is_Leaf(Node.Block);
   end Is_Leaf;


   function Is_Inner
     (Node : Node_Type)
      return Boolean
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      return not Is_Leaf(Node);
   end Is_Inner;


   procedure Set_Parent
     (Node   : in out Node_Type;
      Parent : in     Address_Type)
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      Phys.Set_Parent(Node.Block, Parent);
   end Set_Parent;


   procedure Set_Parent
     (Node   : in out Node_Type;
      Parent : in     Valid_Address_Type)
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      Phys.Set_Parent(Node.Block, To_Address(Parent));
   end Set_Parent;


   function Parent
     (Node : Node_Type)
      return Address_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      return Phys.Parent(Node.Block);
   end Parent;


   function Valid_Parent
     (Node : Node_Type)
      return Valid_Address_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      return To_Valid_Address(Parent(Node));
   end Valid_Parent;


   function Is_Root
     (Node : Node_Type)
      return Boolean
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      return not Is_Valid(Left_Neighbor(Node)) and
             not Is_Valid(Right_Neighbor(Node));
   end Is_Root;


   procedure Set_Left_Neighbor
     (Node     : in out Node_Type;
      Neighbor : in     Address_Type)
   is
      pragma Assert (Node.Ok);
   begin
      Phys.Set_Left(Node.Block, Neighbor);
   end Set_Left_Neighbor;


   procedure Set_Left_Neighbor
     (Node     : in out Node_Type;
      Neighbor : in     Valid_Address_Type)
   is
      pragma Assert (Node.Ok);
   begin
      Phys.Set_Left(Node.Block, To_Address(Neighbor));
   end Set_Left_Neighbor;


   function Left_Neighbor
     (Node : Node_Type)
      return Address_Type
   is
      pragma Assert (Node.Ok);
   begin
      return Phys.Left(Node.Block);
   end Left_Neighbor;


   function Valid_Left_Neighbor
     (Node : Node_Type)
      return Valid_Address_Type
   is
      pragma Assert (Node.Ok);
   begin
      return To_Valid_Address(Phys.Left(Node.Block));
   end Valid_Left_Neighbor;


   procedure Set_Right_Neighbor
     (Node     : in out Node_Type;
      Neighbor : in     Address_Type)
   is
      pragma Assert (Node.Ok);
   begin
      Phys.Set_Right(Node.Block, Neighbor);
   end Set_Right_Neighbor;


   procedure Set_Right_Neighbor
     (Node     : in out Node_Type;
      Neighbor : in     Valid_Address_Type)
   is
      pragma Assert (Node.Ok);
   begin
      Phys.Set_Right(Node.Block, To_Address(Neighbor));
   end Set_Right_Neighbor;


   function Right_Neighbor
     (Node : Node_Type)
      return Address_Type
   is
      pragma Assert (Node.Ok);
   begin
      return Phys.Right(Node.Block);
   end Right_Neighbor;


   function Valid_Right_Neighbor
     (Node : Node_Type)
      return Valid_Address_Type
   is
      pragma Assert (Node.Ok);
   begin
      return To_Valid_Address(Phys.Right(Node.Block));
   end Valid_Right_Neighbor;


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


   function Key
     (Node  : Node_Type;
      Index : Valid_Index_Type)
      return Key_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Key_Context : Key_Context_Type;
      Key         : Key_Type;
      Success     : Boolean;
   begin
      Phys.Read_Key(Key_Context, Node.Block, Index, Key, Success);
      if not Success then
         raise Node_Error;
      end if;
      return Key;
   end Key;


   function Child
     (Node  : Node_Type;
      Index : Valid_Index_Type)
      return Valid_Address_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Inner(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Key_Context : Key_Context_Type;
      Child       : Phys.Child_Type;
      Success     : Boolean;
   begin
      Phys.Read_Child(Key_Context, Node.Block, Index, Child, Success);
      pragma Assert (Is_Valid(Child.Address));
      if not Success then
         raise Node_Error;
      end if;
      return Child.Address;
   end Child;


   function Count
     (Node  : Node_Type;
      Index : Valid_Index_Type)
      return Count_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Inner(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Key_Context : Key_Context_Type;
      Child       : Phys.Child_Type;
      Success     : Boolean;
   begin
      Phys.Read_Child(Key_Context, Node.Block, Index, Child, Success);
      pragma Assert (Is_Valid(Child.Address));
      if not Success then
         raise Node_Error;
      end if;
      return Child.Count;
   end Count;


   function Count_Sum
     (Node : Node_Type)
      return Count_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      if Is_Inner(Node) then
         declare
            C : Count_Type := 0;
         begin
            for I in 1 .. Degree(Node) loop
               C := C + Count(Node, I);
            end loop;
            return C;
         end;
      else
         return Count_Type(Degree(Node));
      end if;
   end Count_Sum;


   function Count_Sum
     (Node     : Node_Type;
      To_Index : Index_Type)
      return Count_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (To_Index <= Degree(Node) + 1);
   begin
      if Is_Inner(Node) then
         declare
            C : Count_Type := 0;
         begin
            for I in 1 .. To_Index loop
               C := C + Count(Node, I);
            end loop;
            return C;
         end;
      else
         return Count_Type(To_Index);
      end if;
   end Count_Sum;


   function Value
     (Node  : Node_Type;
      Index : Valid_Index_Type)
      return Value_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Leaf(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Key_Context   : Key_Context_Type;
      Value_Context : Value_Context_Type;
      Value         : Value_Type;
      Success       : Boolean;
   begin
      Phys.Read_Value(Key_Context, Value_Context, Node.Block, Index,
                      Value, Success);
      if not Success then
         raise Node_Error;
      end if;
      return Value;
   end Value;


   function Key_Position_Binary
     (Node : Node_Type;
      Key  : Key_Type)
      return Index_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      if Degree(Node) > 0 then
         declare
            procedure Find is new Utils.Binary_Search.Find_Best_In_Container
              (Container_Type => Node_Type,
               Index_Type     => Valid_Index_Type,
               Item_Type      => Key_Type,
               Get            => Nodes.Key,
               "<="           => "<=",
               First_Index    => 1,
               Last_Index     => Degree(Node));
            Found : Boolean;
            Index : Index_Type;
         begin
            Find(Node, Key, Found, Index);
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
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      for I in 1 .. Degree(Node) loop
         if Key <= Nodes.Key(Node, I) then
            return I;
         end if;
      end loop;
      return Invalid_Index;
   end Key_Position_Linear;


   function Key_Position
     (Node : Node_Type;
      Key  : Key_Type)
      return Index_Type
   renames Key_Position_Binary;
   pragma Unreferenced (Key_Position_Linear);


   function Count_Position
     (Node      : Node_Type;
      Count_Sum : Count_Type)
      return Index_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      if Is_Inner(Node) then
         declare
            Sum : Count_Type := 0;
         begin
            for I in 1 .. Degree(Node) loop
               Sum := Sum + Count(Node, I);
               if Count_Sum <= Sum then
                  return I;
               end if;
            end loop;
            return Invalid_Index;
         end;
      else
         return Index_Type(Count_Sum);
      end if;
   end Count_Position;


   function Child_Position
     (Node  : Node_Type;
      Child : Valid_Address_Type)
      return Valid_Index_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
   begin
      for I in 1 .. Degree(Node) loop
         if Child = Nodes.Child(Node, I) then
            return I;
         end if;
      end loop;
      raise Node_Error;
   end Child_Position;


   function Split_Position
     (Node : Node_Type)
      return Valid_Index_Type
   is
      use type IO.Blocks.Long_Position_Type;

      function Min is new Utils.Gen_Minimum(IO.Blocks.Long_Position_Type);
      pragma Inline (Min);

      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));

      Total    : constant IO.Blocks.Long_Position_Type
               := Phys.Entries_Size(Node.Block);
      Sum      : IO.Blocks.Long_Position_Type := 0;
      Prev_Min : IO.Blocks.Long_Position_Type := 0;
   begin
      for I in 1 .. Degree(Node) loop
         Sum := Sum + Phys.Entry_Size(Node.Block, I);
         declare
            This_Min : constant IO.Blocks.Long_Position_Type
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
     (Left_Node  : Node_Type;
      Right_Node : Node_Type)
      return Valid_Index_Type
   is
      use type IO.Blocks.Size_Type;

      function Min is new Utils.Gen_Minimum(IO.Blocks.Size_Type);
      pragma Inline (Min);

      pragma Assert (Left_Node.Ok);
      pragma Assert (Right_Node.Ok);
      pragma Assert (not Is_Free(Left_Node));
      pragma Assert (not Is_Free(Right_Node));

      Total    : constant IO.Blocks.Size_Type
               := IO.Blocks.Size_Type(Phys.Entries_Size(Left_Node.Block)) +
                  IO.Blocks.Size_Type(Phys.Entries_Size(Right_Node.Block));
      Sum      : IO.Blocks.Size_Type := 0;
      Prev_Min : IO.Blocks.Size_Type := 0;
   begin
      for I in 1 .. Degree(Left_Node) loop
         Sum := Sum + IO.Blocks.Size_Type(Phys.Entry_Size(Left_Node.Block, I));
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
         Sum := Sum + IO.Blocks.Size_Type(Phys.Entry_Size(Right_Node.Block, I));
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


   function Is_Valid
     (Index : Index_Type)
      return Boolean is
   begin
      return Index /= Invalid_Index;
   end Is_Valid;


   procedure Set_Count
     (Node  : in out Node_Type;
      Index : in     Valid_Index_Type;
      Count : in     Count_Type)
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Old_Child_Addr : constant Valid_Address_Type := Nodes.Child(Node, Index);
      Child : Phys.Child_Type;
   begin
      declare
         Key_Context : Key_Context_Type;
         Success     : Boolean;
      begin
         Phys.Read_Child(Key_Context, Node.Block, Index, Child, Success);
         pragma Assert (Is_Valid(Child.Address));
         if not Success then
            raise Node_Error;
         end if;
      end;
      Child.Count := Count;
      declare
         Key_Context : Key_Context_Type;
      begin
         Phys.Write_Child(Key_Context, Node.Block, Index, Child, Node.Ok);
         if not Node.Ok then
            raise Node_Error;
         end if;
      end;
      pragma Assert (Old_Child_Addr = Nodes.Child(Node, Index));
   end Set_Count;


   procedure Set_Child_And_Count
     (Node  : in out Node_Type;
      Index : in     Valid_Index_Type;
      Child : in     Valid_Address_Type;
      Count : in     Count_Type)
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Index in 1 .. Degree(Node));
      pragma Assert (Is_Valid(Child));

      C           : constant Phys.Child_Type := Phys.Child_Type'(Child, Count);
      Key_Context : Key_Context_Type;
   begin
      Phys.Write_Child(Key_Context, Node.Block, Index, C, Node.Ok);
      if not Node.Ok then
         raise Node_Error;
      end if;
   end Set_Child_And_Count;


   procedure Copy_Entry
     (Node              : in out Node_Type;
      Source            : in     Node_Type;
      Index             : in     Valid_Index_Type;
      Key_Read_Context  : in out Key_Context_Type;
      Key_Write_Context : in out Key_Context_Type;
      Shift_By          : in     Integer := 0)
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Inner(Node));

      New_Index : constant Valid_Index_Type
                := Valid_Index_Type(Integer(Index) + Shift_By);
      Key       : Key_Type;
      Child     : Phys.Child_Type;
      Success   : Boolean;
   begin
      Phys.Read_Entry(Key_Read_Context, Source.Block, Index, Key, Child,
                      Success);
      if not Success then
         raise Node_Error;
      end if;
      pragma Assert (Is_Valid(Child.Address));
      Phys.Write_Entry(Key_Write_Context, Node.Block, New_Index, Key, Child,
                       Node.Ok);
   end Copy_Entry;


   procedure Copy_Entry
     (Node                : in out Node_Type;
      Source              : in     Node_Type;
      Index               : in     Valid_Index_Type;
      Key_Read_Context    : in out Key_Context_Type;
      Key_Write_Context   : in out Key_Context_Type;
      Value_Read_Context  : in out Value_Context_Type;
      Value_Write_Context : in out Value_Context_Type;
      Shift_By            : in     Integer := 0)
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Leaf(Node));

      New_Index : constant Valid_Index_Type
                := Valid_Index_Type(Integer(Index) + Shift_By);
      Key       : Key_Type;
      Value     : Value_Type;
      Success   : Boolean;
   begin
      Phys.Read_Entry(Key_Read_Context, Value_Read_Context, Source.Block, Index,
                      Key, Value, Success);
      if not Success then
         raise Node_Error;
      end if;
      Phys.Write_Entry(Key_Write_Context, Value_Write_Context,
                       Node.Block, New_Index, Key, Value, Node.Ok);
   end Copy_Entry;


   function Insertion
     (Node  : Node_Type;
      Index : Valid_Index_Type;
      Key   : Key_Type;
      Child : Valid_Address_Type;
      Count : Count_Type)
      return Node_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Inner(Node));

      N : Node_Type := (Ok    => True,
                        Block => Phys.New_Block(Is_Free => Is_Free(Node),
                                              Degree  => Degree(Node) + 1,
                                              Is_Leaf => Is_Leaf(Node),
                                              Parent  => Parent(Node),
                                              Left    => Left_Neighbor(Node),
                                              Right   => Right_Neighbor(Node)));
      Key_Read_Context  : Key_Context_Type;
      Key_Write_Context : Key_Context_Type;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context);
         if not N.Ok then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, N.Block, Index, Key,
                       Phys.Child_Type'(Child, Count), N.Ok);
      if not N.Ok then
         return N;
      end if;

      for I in Index .. Degree(Node) loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context, +1);
         if not N.Ok then
            return N;
         end if;
      end loop;
      return N;
   end Insertion;


   function Insertion
     (Node  : Node_Type;
      Index : Valid_Index_Type;
      Key   : Key_Type;
      Value : Value_Type)
      return Node_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Leaf(Node));

      N : Node_Type := (Ok    => True,
                        Block => Phys.New_Block(Is_Free => Is_Free(Node),
                                              Degree  => Degree(Node) + 1,
                                              Is_Leaf => Is_Leaf(Node),
                                              Parent  => Parent(Node),
                                              Left    => Left_Neighbor(Node),
                                              Right   => Right_Neighbor(Node)));
      Key_Read_Context    : Key_Context_Type;
      Key_Write_Context   : Key_Context_Type;
      Value_Read_Context  : Value_Context_Type;
      Value_Write_Context : Value_Context_Type;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                    Value_Read_Context, Value_Write_Context);
         if not N.Ok then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, Value_Write_Context, N.Block,
                       Index, Key, Value, N.Ok);
      if not N.Ok then
         return N;
      end if;

      for I in Index .. Degree(Node) loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                    Value_Read_Context, Value_Write_Context, +1);
         if not N.Ok then
            return N;
         end if;
      end loop;
      return N;
   end Insertion;


   function Substitution
     (Node  : Node_Type;
      Index : Valid_Index_Type;
      Key   : Key_Type;
      Child : Valid_Address_Type;
      Count : Count_Type)
      return Node_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Inner(Node));
      pragma Assert (Is_Valid(Child));

      N : Node_Type := (Ok    => True,
                        Block => Phys.New_Block(Is_Free => Is_Free(Node),
                                              Degree  => Degree(Node),
                                              Is_Leaf => Is_Leaf(Node),
                                              Parent  => Parent(Node),
                                              Left    => Left_Neighbor(Node),
                                              Right   => Right_Neighbor(Node)));
      Key_Read_Context  : Key_Context_Type;
      Key_Write_Context : Key_Context_Type;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context);
         if not N.Ok then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, N.Block, Index, Key,
                       Phys.Child_Type'(Child, Count), N.Ok);
      if not N.Ok then
         return N;
      end if;

      for I in Index + 1 .. Degree(Node) loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context);
         if not N.Ok then
            return N;
         end if;
      end loop;
      return N;
   end Substitution;


   function Substitution
     (Node  : Node_Type;
      Index : Valid_Index_Type;
      Key   : Key_Type;
      Value : Value_Type)
      return Node_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));
      pragma Assert (Is_Leaf(Node));

      N : Node_Type := (Ok    => True,
                        Block => Phys.New_Block(Is_Free => Is_Free(Node),
                                              Degree  => Degree(Node),
                                              Is_Leaf => Is_Leaf(Node),
                                              Parent  => Parent(Node),
                                              Left    => Left_Neighbor(Node),
                                              Right   => Right_Neighbor(Node)));
      Key_Read_Context    : Key_Context_Type;
      Key_Write_Context   : Key_Context_Type;
      Value_Read_Context  : Value_Context_Type;
      Value_Write_Context : Value_Context_Type;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                    Value_Read_Context, Value_Write_Context);
         if not N.Ok then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, Value_Write_Context, N.Block, Index,
                       Key, Value, N.Ok);
      if not N.Ok then
         return N;
      end if;

      for I in Index + 1 .. Degree(Node) loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                    Value_Read_Context, Value_Write_Context);
         if not N.Ok then
            return N;
         end if;
      end loop;
      return N;
   end Substitution;


   function Deletion
     (Node  : Node_Type;
      Index : Valid_Index_Type)
      return Node_Type
   is
      pragma Assert (Node.Ok);
      pragma Assert (not Is_Free(Node));

      N : Node_Type := (Ok    => True,
                        Block => Phys.New_Block(Is_Free => Is_Free(Node),
                                              Degree  => Degree(Node) - 1,
                                              Is_Leaf => Is_Leaf(Node),
                                              Parent  => Parent(Node),
                                              Left    => Left_Neighbor(Node),
                                              Right   => Right_Neighbor(Node)));
   begin
      if Is_Inner(Node) then
         declare
            Key_Read_Context  : Key_Context_Type;
            Key_Write_Context : Key_Context_Type;
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
            Key_Read_Context    : Key_Context_Type;
            Key_Write_Context   : Key_Context_Type;
            Value_Read_Context  : Value_Context_Type;
            Value_Write_Context : Value_Context_Type;
         begin
            for I in 1 .. Index - 1 loop
               Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                          Value_Read_Context, Value_Write_Context);
               if not N.Ok then
                  return N;
               end if;
            end loop;
            for I in Index + 1 .. Degree(Node) loop
               Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                          Value_Read_Context, Value_Write_Context, -1);
               if not N.Ok then
                  return N;
               end if;
            end loop;
         end;
      end if;
      return N;
   end Deletion;


   function Copy
     (Node : Node_Type;
      From : Valid_Index_Type;
      To   : Index_Type)
      return Node_Type is
   begin
      if not Node.Ok then
         declare
            Not_Ok_Node : Node_Type;
         begin
            Not_Ok_Node.Ok := False;
            return Not_Ok_Node;
         end;
      end if;

      declare
         pragma Assert (not Is_Free(Node));
         N : Node_Type := (Ok    => True,
                           Block => Phys.New_Block
                                       (Is_Free => Is_Free(Node),
                                        Degree  => To - From + 1,
                                        Is_Leaf => Is_Leaf(Node),
                                        Parent  => Parent(Node),
                                        Left    => Left_Neighbor(Node),
                                        Right   => Right_Neighbor(Node)));
         Shift_By : constant Integer := -1 * Integer(From) + 1;
      begin
         if Is_Inner(Node) then
            declare
               Key_Read_Context  : Key_Context_Type;
               Key_Write_Context : Key_Context_Type;
            begin
               for I in From .. To loop
                  Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                             Shift_By);
                  if not N.Ok then
                     return N;
                  end if;
               end loop;
            end;
         else
            declare
               Key_Read_Context    : Key_Context_Type;
               Key_Write_Context   : Key_Context_Type;
               Value_Read_Context  : Value_Context_Type;
               Value_Write_Context : Value_Context_Type;
            begin
               for I in From .. To loop
                  Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                             Value_Read_Context, Value_Write_Context, Shift_By);
                  if not N.Ok then
                     return N;
                  end if;
               end loop;
            end;
         end if;
         return N;
      end;
   end Copy;


   function Combi_Copy
     (Left_Node  : Node_Type;
      Right_Node : Node_Type;
      From       : Valid_Index_Type;
      To         : Index_Type)
      return Node_Type is
   begin
      if not Left_Node.Ok or not Right_Node.Ok then
         declare
            Not_Ok_Node : Node_Type;
         begin
            Not_Ok_Node.Ok := False;
            return Not_Ok_Node;
         end;
      end if;

      declare
         pragma Assert (not Is_Free(Left_Node));
         pragma Assert (not Is_Free(Right_Node));

         function Min is new Utils.Gen_Minimum(Index_Type);
         pragma Inline (Min);
         function Max is new Utils.Gen_Maximum(Index_Type);
         pragma Inline (Max);

         function Combi_Parent
           (Left_Node  : Node_Type;
            Right_Node : Node_Type;
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
           (Left_Node  : Node_Type;
            Right_Node : Node_Type;
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
           (Left_Node  : Node_Type;
            Right_Node : Node_Type;
            To         : Index_Type)
            return Address_Type is
         begin
            if To <= Degree(Left_Node) then
               return Right_Neighbor(Left_Node);
            else
               return Right_Neighbor(Right_Node);
            end if;
         end Combi_Right_Neighbor;

         N : Node_Type
           := (Ok    => True,
               Block => Phys.New_Block
                 (Is_Free => Is_Free(Left_Node),
                  Degree  => To - From + 1,
                  Is_Leaf => Is_Leaf(Left_Node),
                  Parent  => Combi_Parent(Left_Node, Right_Node, To),
                  Left    => Combi_Left_Neighbor(Left_Node, Right_Node, From),
                  Right   => Combi_Right_Neighbor(Left_Node, Right_Node, To)));
         Left_Shift_By  : constant Integer
                        := -1 * Integer(From) + 1;
         Right_Shift_By : constant Integer
                        := -1 * Integer(From) + 1 + Integer(Degree(Left_Node));
      begin
         if Is_Inner(Left_Node) then
            declare
               Key_Read_Context  : Key_Context_Type;
               Key_Write_Context : Key_Context_Type;
            begin
               for I in From .. Min(To, Degree(Left_Node)) loop
                  Copy_Entry(N, Left_Node, I, Key_Read_Context,
                             Key_Write_Context, Left_Shift_By);
                  if not N.Ok then
                     return N;
                  end if;
               end loop;
               for I in Max(From, Degree(Left_Node) + 1) .. To loop
                  Copy_Entry(N, Right_Node, I - Degree(Left_Node),
                             Key_Read_Context, Key_Write_Context,
                             Right_Shift_By);
                  if not N.Ok then
                     return N;
                  end if;
               end loop;
            end;
         else
            declare
               Key_Read_Context    : Key_Context_Type;
               Key_Write_Context   : Key_Context_Type;
               Value_Read_Context  : Value_Context_Type;
               Value_Write_Context : Value_Context_Type;
            begin
               for I in From .. Min(To, Degree(Left_Node)) loop
                  Copy_Entry(N, Left_Node, I, Key_Read_Context,
                             Key_Write_Context, Value_Read_Context,
                             Value_Write_Context, Left_Shift_By);
                  if not N.Ok then
                     return N;
                  end if;
               end loop;
               for I in Max(From, Degree(Left_Node) + 1) .. To loop
                  Copy_Entry(N, Right_Node, I - Degree(Left_Node),
                             Key_Read_Context, Key_Write_Context,
                             Value_Read_Context, Value_Write_Context,
                             Right_Shift_By);
                  if not N.Ok then
                     return N;
                  end if;
               end loop;
            end;
         end if;
         return N;
      end;
   end Combi_Copy;


   function Combination
     (Left_Node  : Node_Type;
      Right_Node : Node_Type)
      return Node_Type is
   begin
      if not Left_Node.Ok or not Right_Node.Ok then
         declare
            Not_Ok_Node : Node_Type;
         begin
            Not_Ok_Node.Ok := False;
            return Not_Ok_Node;
         end;
      end if;

      declare
         pragma Assert (Left_Node.Ok);
         pragma Assert (Right_Node.Ok);
         pragma Assert (Validation(Left_Node) /= Too_Large or
                        Validation(Right_Node) /= Too_Large);
         pragma Assert (not Is_Free(Left_Node));
         pragma Assert (not Is_Free(Right_Node));
         pragma Assert (Is_Leaf(Left_Node) = Is_Leaf(Right_Node));

         Left_Degree  : constant Degree_Type := Degree(Left_Node);
         Right_Degree : constant Degree_Type := Degree(Right_Node);
         Degree       : constant Degree_Type := Left_Degree + Right_Degree;
         N : Node_Type := (Ok    => True,
                           Block => Phys.New_Block
                                       (Is_Free => Is_Free(Right_Node),
                                        Degree  => Degree,
                                        Is_Leaf => Is_Leaf(Right_Node),
                                        Parent  => Parent(Right_Node),
                                        Left    => Left_Neighbor(Left_Node),
                                        Right   => Right_Neighbor(Right_Node)));
      begin
         if Is_Inner(Right_Node) then
            declare
               Key_Read_Context  : Key_Context_Type;
               Key_Write_Context : Key_Context_Type;
            begin
               for I in 1 .. Left_Degree loop
                  Copy_Entry(N, Left_Node, I, Key_Read_Context,
                             Key_Write_Context);
                  if not N.Ok then
                     return N;
                  end if;
               end loop;
               for I in 1 .. Right_Degree loop
                  Copy_Entry(N, Right_Node, I, Key_Read_Context,
                             Key_Write_Context, +1 * Integer(Left_Degree));
                  if not N.Ok then
                     return N;
                  end if;
               end loop;
            end;
         else
            declare
               Key_Read_Context    : Key_Context_Type;
               Key_Write_Context   : Key_Context_Type;
               Value_Read_Context  : Value_Context_Type;
               Value_Write_Context : Value_Context_Type;
            begin
               for I in 1 .. Left_Degree loop
                  Copy_Entry(N, Left_Node, I, Key_Read_Context,
                             Key_Write_Context, Value_Read_Context,
                             Value_Write_Context);
                  if not N.Ok then
                     return N;
                  end if;
               end loop;
               for I in 1 .. Right_Degree loop
                  Copy_Entry(N, Right_Node, I, Key_Read_Context,
                             Key_Write_Context, Value_Read_Context,
                             Value_Write_Context, +1 * Integer(Left_Degree));
                  if not N.Ok then
                     return N;
                  end if;
               end loop;
            end;
         end if;
         return N;
      end;
   end Combination;


   function Size_Of
     (Node : Node_Type)
      return IO.Blocks.Size_Type
   is
      pragma Assert (Node.Ok);
   begin
      return IO.Blocks.Size_Type(Phys.Total_Size(Node.Block));
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
      return Validation_Result_Type
   is
      use type IO.Blocks.Size_Type;
   begin
      if not Node.Ok then
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
         Needed     : constant IO.Blocks.Long_Position_Type
                    := Phys.Entries_Size(Node.Block);
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
      use type IO.Blocks.Long_Position_Type;
      Max_Entry_Size        : constant IO.Blocks.Long_Position_Type
                            := Phys.Effective_Block_Space * 1 / 4;
      Max_Value_Size_As_Pos : constant IO.Blocks.Position_Type
                            := IO.Blocks.Long_Position_Type(Max_Value_Size);
   begin
      return IO.Blocks.Size_Type
               (Phys.Max_Key_Size(Max_Entry_Size => Max_Entry_Size,
                                  Max_Value_Size => Max_Value_Size_As_Pos));
   end Max_Key_Size;


   function To_Block
     (Node : Node_Type)
      return IO.Blocks.Block_Type
   is
      pragma Assert (Node.Ok);
      use type IO.Blocks.Base_Position_Type;
   begin
      pragma Assert (Phys.Total_Size(Node.Block) <= IO.Blocks.Block_Size);
      pragma Assert (Validation(Node) = Valid);
      return IO.Blocks.To_Block(Node.Block, Phys.Total_Size(Node.Block));
   end To_Block;


   function From_Block
     (Block : IO.Blocks.Block_Type)
      return Node_Type
   is
      Node : constant Node_Type
           := (Ok => True, Block => IO.Blocks.To_Long_Block(Block));
      use type IO.Blocks.Base_Position_Type;
   begin
      pragma Assert (Phys.Total_Size(Node.Block) <= IO.Blocks.Block_Size);
      pragma Assert (Validation(Node) = Valid);
      return Node;
   end From_Block;

end Nodes;

