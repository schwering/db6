-- Abstract:
--
-- Nodes are directly represented as Blocks. Writable nodes have some overflow
-- storage space.
-- Each node has a degree, which is 0 if it is empty. Furthermore, it is either
-- a inner node or a leaf node. Additionally, it has a link which may point to
-- its right neighbor if it exists.
-- Non-empty inner and leaf nodes contain some keys and child node addresses (in
-- case of inner nodes) or values (leaf nodes).
-- Empty nodes however may either contain absolutely nothing but their meta
-- data, or they may have a high key. When the node never held any key and still
-- doesn't, it has no high key. Otherwise, the high key is the (temporally) last
-- key contained in the node.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils.Binary_Search;

separate (DB.Gen_BTrees)
package body Nodes is

   -- The layout of a Block, i.e. a byte sequence is the following:
   -- 1. Meta_Data
   -- a) If Degree(N) > 0:
   --    2. If Is_Leaf(N): Degree(N) times Index_Type (end position of entry)
   --                      where an entry is either (Key, Child) or (Key, Value)
   --    3. If Is_Leaf(N): Key1, Value1, ..., Key_|N|, Value_|N|
   --       Else:          Key1, Child_Addr_1, ..., Key_|N|, Child_Addr_|N|
   -- b) If Degree(N) = 0:
   --    2. Index_Type (end position of high key)
   --    3. If Has_High_Key(N): High_Key
   package Phys is
      pragma Elaborate_Body;

      function Entry_Size
        (Block : Blocks.Base_Block_Type;
         Index : Valid_Index_Type)
         return Blocks.Base_Position_Type;

      function Entries_Size
        (Block : Blocks.Base_Block_Type)
         return Blocks.Base_Position_Type;

      function Total_Size
        (Block : Blocks.Base_Block_Type)
         return Blocks.Base_Position_Type;

      function Effective_Block_Space
        return Blocks.Base_Position_Type;

      function Max_Key_Size
        (Max_Entry_Size : Blocks.Base_Position_Type;
         Max_Value_Size : Blocks.Base_Position_Type)
         return Blocks.Base_Position_Type;

      function Last_Used_Index
        (Block : Blocks.Base_Block_Type)
         return Blocks.Base_Index_Type;

      procedure Init_Block
        (Block   : in out Blocks.Base_Block_Type;
         Is_Ok   : in     Boolean;
         Level   : in     Level_Type;
         Degree  : in     Degree_Type;
         Link    : in     Address_Type);

      function Is_Ok
        (Block : Blocks.Base_Block_Type)
         return Boolean;

      function Has_High_Key
        (Block : Blocks.Base_Block_Type)
         return Boolean;

      procedure Set_Has_High_Key
        (Block        : in out Blocks.Base_Block_Type;
         Has_High_Key : in     Boolean);

      function Level
        (Block : Blocks.Base_Block_Type)
         return Level_Type;

      function Degree
        (Block : Blocks.Base_Block_Type)
         return Degree_Type;

      function Link
        (Block : Blocks.Base_Block_Type)
         return Address_Type;

      procedure Set_Link
        (Block   : in out Blocks.Base_Block_Type;
         Address : in     Address_Type);

      procedure Read_Key
        (Key_Context : in out Keys.Read_Context_Type;
         Block       : in     Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Keys.Key_Type;
         Success     :    out Boolean);

      procedure Read_Child
        (Key_Context : in out Keys.Read_Context_Type;
         Block       : in     Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Child       :    out Valid_Address_Type;
         Success     :    out Boolean);

      procedure Read_Value
        (Key_Context   : in out Keys.Read_Context_Type;
         Value_Context : in out Values.Read_Context_Type;
         Block         : in     Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Value         :    out Values.Value_Type;
         Success       :    out Boolean);

      procedure Read_Entry
        (Key_Context : in out Keys.Read_Context_Type;
         Block       : in     Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Keys.Key_Type;
         Child       :    out Valid_Address_Type;
         Success     :    out Boolean);

      procedure Read_Entry
        (Key_Context   : in out Keys.Read_Context_Type;
         Value_Context : in out Values.Read_Context_Type;
         Block         : in     Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           :    out Keys.Key_Type;
         Value         :    out Values.Value_Type;
         Success       :    out Boolean);

      procedure Read_High_Key
        (Key_Context : in out Keys.Read_Context_Type;
         Block       : in     Blocks.Base_Block_Type;
         High_Key    :    out Keys.Key_Type;
         Success     :    out Boolean);

      procedure Write_Entry
        (Key_Context : in out Keys.Write_Context_Type;
         Block       : in out Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         : in     Keys.Key_Type;
         Child       : in     Valid_Address_Type);

      procedure Write_Entry
        (Key_Context   : in out Keys.Write_Context_Type;
         Value_Context : in out Values.Write_Context_Type;
         Block         : in out Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           : in     Keys.Key_Type;
         Value         : in     Values.Value_Type);

      procedure Write_High_Key
        (Key_Context : in out Keys.Write_Context_Type;
         Block       : in out Blocks.Base_Block_Type;
         High_Key    : in     Keys.Key_Type);

      private
         pragma Inline (Entry_Size);
         pragma Inline (Entries_Size);
         pragma Inline (Total_Size);
         pragma Inline (Effective_Block_Space);
         pragma Inline (Last_Used_Index);
         pragma Inline (Init_Block);
         pragma Inline (Is_Ok);
         pragma Inline (Degree);
         pragma Inline (Link);
         pragma Inline (Read_Key);
         pragma Inline (Read_Child);
         pragma Inline (Read_Value);
         pragma Inline (Read_Entry);
         pragma Inline (Read_High_Key);
         pragma Inline (Write_Entry);
         pragma Inline (Write_High_Key);
   end Phys;


   package body Phys is
      use type Blocks.Base_Position_Type;

      type Booleans_Type is
         record
            Is_Ok        : Boolean;
            Has_High_Key : Boolean;
         end record;
      pragma Pack (Booleans_Type);

      function Size_Of is new Blocks.Size_Of(Booleans_Type);
      function Size_Of is new Blocks.Size_Of(Level_Type);
      function Size_Of is new Blocks.Size_Of(Degree_Type);
      function Size_Of is new Blocks.Size_Of(Address_Type);
      function Size_Of is new Blocks.Size_Of(Blocks.Position_Type);
      function Size_Of is new Blocks.Size_Of(Valid_Address_Type);

      Size_Of_Booleans  : constant Blocks.Base_Position_Type :=
         Blocks.Base_Position_Type(Size_Of(Booleans_Type'(others => <>)));

      Size_Of_Level     : constant Blocks.Base_Position_Type :=
         Blocks.Base_Position_Type(Size_Of(Level_Type'First));

      Size_Of_Degree    : constant Blocks.Base_Position_Type :=
         Blocks.Base_Position_Type(Size_Of(Degree_Type'First));

      Size_Of_Address   : constant Blocks.Base_Position_Type :=
         Blocks.Base_Position_Type(Size_Of(Invalid_Address));

      Size_Of_Position  : constant  Blocks.Base_Position_Type :=
         Blocks.Base_Position_Type(Size_Of(Blocks.Position_Type'Last));

      Size_Of_Child     : constant Blocks.Base_Position_Type :=
         Blocks.Base_Position_Type(Size_Of(Valid_Address_Type(Block_IO.First)));

      Size_Of_Meta_Data : constant Blocks.Base_Position_Type :=
         Size_Of_Booleans + Size_Of_Level + Size_Of_Degree + Size_Of_Address;

      -- Layout of Node Blocks is as follows:
      -- 1. Is_Ok/Has_High_Key             (Size_Of_Booleans)
      -- 2. Level                          (Size_Of_Level)
      -- 3. Degree                         (Size_Of_Degree)
      -- 4. Link                           (Size_Of_Address)
      -- 5. Indexes                        (|Degree| * Size_Of_Position)
      -- 6. Entries                        (Size_Of(Key_1) + Size_Of_Child ..
      --                                    Size_Of(Key_N) + Size_Of_Child)
      --                                or (Size_Of(Key_1) + Size_Of(Value_1) ..
      --                                    Size_Of(Key_N) + Size_Of(Value_N))


      function "*"
        (I : Valid_Index_Type;
         J : Blocks.Base_Index_Type)
         return Blocks.Base_Index_Type
      is
         pragma Inline ("*");
      begin
         return Blocks.Base_Index_Type(I) * J;
      end "*";


      function Pos_From_Pos
        (Entry_Index : Valid_Index_Type)
         return Blocks.Base_Index_Type
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
         return Blocks.Base_Index_Type
      is
         pragma Inline (Pos_To_Pos);
      begin
         return Size_Of_Meta_Data + Entry_Index * Size_Of_Position;
      end Pos_To_Pos;


      function Entry_To_Pos
        (Block       : Blocks.Base_Block_Type;
         Entry_Index : Valid_Index_Type)
         return Blocks.Base_Index_Type
      is
         pragma Inline (Entry_To_Pos);
         pragma Assert ((Entry_Index = 1 and Degree(Block) = 0) or
                        Entry_Index in 1 .. Degree(Block));
         procedure Read is new Blocks.Read_At(Blocks.Base_Index_Type);
         Index : Blocks.Base_Index_Type;
      begin
         Read(Block, Pos_From_Pos(Entry_Index), Pos_To_Pos(Entry_Index), Index);
         pragma Assert (Index'Valid);
         return Index;
      end Entry_To_Pos;


      function Entry_From_Pos
        (Block       : Blocks.Base_Block_Type;
         Entry_Index : Valid_Index_Type)
         return Blocks.Base_Position_Type
      is
         pragma Inline (Entry_From_Pos);
         pragma Assert ((Entry_Index = 1 and Degree(Block) = 0) or
                        Entry_Index in 1 .. Degree(Block));
      begin
         if Degree(Block) = 0 then
            return Size_Of_Meta_Data + Size_Of_Position + 1;
         elsif Entry_Index = 1 then
            return Size_Of_Meta_Data + Degree(Block) * Size_Of_Position + 1;
         else
            return Entry_To_Pos(Block, Entry_Index - 1) + 1;
         end if;
      end Entry_From_Pos;


      function New_Cursor_From
        (Block : Blocks.Base_Block_Type;
         Index : Valid_Index_Type)
         return Blocks.Cursor_Type
      is
         pragma Inline (New_Cursor_From);
      begin
         return Blocks.New_Cursor(Entry_From_Pos(Block, Index));
      end New_Cursor_From;


      procedure Set_Entry_Size
        (Block         : in out Blocks.Base_Block_Type;
         Entry_Index   : in     Valid_Index_Type;
         Raw_Data_Size : in     Blocks.Base_Position_Type)
      is
         pragma Inline (Set_Entry_Size);
         pragma Assert ((Entry_Index = 1 and Degree(Block) = 0) or
                        Entry_Index in 1 .. Degree(Block));
         procedure Write is new Blocks.Write_At(Blocks.Base_Index_Type);
         From : constant Blocks.Base_Index_Type :=
            Entry_From_Pos(Block, Entry_Index);
         To   : constant Blocks.Base_Index_Type :=
            From + Raw_Data_Size - 1;
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
        (Block : Blocks.Base_Block_Type;
         Index : Valid_Index_Type)
         return Blocks.Base_Position_Type is
      begin
         return Entry_To_Pos(Block, Index) - Entry_From_Pos(Block, Index) + 1 +
                Size_Of_Position;
      end Entry_Size;


      function Entries_Size
        (Block : Blocks.Base_Block_Type)
         return Blocks.Base_Position_Type is
      begin
         if Degree(Block) = 0 then
            if Has_High_Key(Block) then
               return Entry_To_Pos(Block, 1) - Size_Of_Meta_Data;
            else
               return 0;
            end if;
         else
            return Entry_To_Pos(Block, Degree(Block)) - Size_Of_Meta_Data;
         end if;
      end Entries_Size;


      function Total_Size
        (Block : Blocks.Base_Block_Type)
         return Blocks.Base_Position_Type is
      begin
         return Size_Of_Meta_Data + Entries_Size(Block);
      end Total_Size;


      function Effective_Block_Space
        return Blocks.Base_Position_Type is
      begin
         return Blocks.Index_Type'Last - Size_Of_Meta_Data;
      end Effective_Block_Space;


      function Max_Key_Size
        (Max_Entry_Size : Blocks.Base_Position_Type;
         Max_Value_Size : Blocks.Base_Position_Type)
         return Blocks.Base_Position_Type is
      begin
         if Size_Of_Child > Max_Value_Size then
            return Max_Entry_Size - Size_Of_Child - Size_Of_Position;
         else
            return Max_Entry_Size - Max_Value_Size - Size_Of_Position;
         end if;
      end Max_Key_Size;


      function Last_Used_Index
        (Block : Blocks.Base_Block_Type)
         return Blocks.Base_Index_Type is
      begin
         if Degree(Block) = 0 then
            return Blocks.Base_Index_Type(Size_Of_Meta_Data);
         else
            return Entry_To_Pos(Block, Degree(Block));
         end if;
      end Last_Used_Index;


      function Free_Space
        (Block : Blocks.Base_Block_Type)
         return Blocks.Base_Position_Type
      is
         pragma Inline (Free_Space);
      begin
         return Blocks.Base_Index_Type'Last - Last_Used_Index(Block);
      end Free_Space;
      pragma Unreferenced (Free_Space);


      function Is_Ok
        (Block : Blocks.Base_Block_Type)
         return Boolean
      is
         procedure Read is new Blocks.Read_At(Booleans_Type);
         Offset   : constant Blocks.Base_Position_Type := 0;
         From     : constant Blocks.Base_Index_Type :=
            Blocks.Base_Index_Type'First + Offset;
         Booleans : Booleans_Type;
      begin
         Read(Block, From, From + Size_Of_Booleans - 1, Booleans);
         return Booleans.Is_Ok;
      end Is_Ok;


      procedure Set_Ok
        (Block : in out Blocks.Base_Block_Type;
         Is_Ok : in     Boolean)
      is
         procedure Read is new Blocks.Read_At(Booleans_Type);
         procedure Write is new Blocks.Write_At(Booleans_Type);
         Offset   : constant Blocks.Base_Position_Type := 0;
         From     : constant Blocks.Base_Index_Type :=
            Blocks.Base_Index_Type'First + Offset;
         Booleans : Booleans_Type;
      begin
         Read(Block, From, From + Size_Of_Booleans - 1, Booleans);
         Booleans.Is_Ok := Is_Ok;
         Write(Block, From, From + Size_Of_Booleans - 1, Booleans);
      end Set_Ok;


      function Has_High_Key
        (Block : Blocks.Base_Block_Type)
         return Boolean
      is
         procedure Read is new Blocks.Read_At(Booleans_Type);
         Offset   : constant Blocks.Base_Position_Type := 0;
         From     : constant Blocks.Base_Index_Type :=
            Blocks.Base_Index_Type'First + Offset;
         Booleans : Booleans_Type;
      begin
         Read(Block, From, From + Size_Of_Booleans - 1, Booleans);
         return Booleans.Has_High_Key;
      end Has_High_Key;


      procedure Set_Has_High_Key
        (Block        : in out Blocks.Base_Block_Type;
         Has_High_Key : in     Boolean)
      is
         pragma Assert (not Has_High_Key or Degree(Block) = 0);
         procedure Read is new Blocks.Read_At(Booleans_Type);
         procedure Write is new Blocks.Write_At(Booleans_Type);
         Offset   : constant Blocks.Base_Position_Type := 0;
         From     : constant Blocks.Base_Index_Type :=
            Blocks.Base_Index_Type'First + Offset;
         Booleans : Booleans_Type;
      begin
         Read(Block, From, From + Size_Of_Booleans - 1, Booleans);
         Booleans.Has_High_Key := Has_High_Key;
         Write(Block, From, From + Size_Of_Booleans - 1, Booleans);
      end Set_Has_High_Key;


      function Level
        (Block : Blocks.Base_Block_Type)
         return Level_Type
      is
         procedure Read is new Blocks.Read_At(Level_Type);
         Offset : constant Blocks.Base_Position_Type := Size_Of_Booleans;
         From   : constant Blocks.Base_Index_Type :=
            Blocks.Base_Index_Type'First + Offset;
         Level : Level_Type;
      begin
         Read(Block, From, From + Size_Of_Level - 1, Level);
         pragma Assert (Level'Valid);
         return Level;
      end Level;


      procedure Set_Level
        (Block : in out Blocks.Base_Block_Type;
         Level : in     Level_Type)
      is
         procedure Write is new Blocks.Write_At(Level_Type);
         Offset : constant Blocks.Base_Position_Type := Size_Of_Booleans;
         From   : constant Blocks.Base_Index_Type :=
            Blocks.Base_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Level - 1, Level);
         pragma Assert (Level = Phys.Level(Block));
      end Set_Level;


      function Degree
        (Block : Blocks.Base_Block_Type)
         return Degree_Type
      is
         procedure Read is new Blocks.Read_At(Degree_Type);
         Offset : constant Blocks.Base_Position_Type :=
            Size_Of_Booleans + Size_Of_Level;
         From   : constant Blocks.Base_Index_Type :=
            Blocks.Base_Index_Type'First + Offset;
         Degree : Degree_Type;
      begin
         Read(Block, From, From + Size_Of_Degree - 1, Degree);
         pragma Assert (Degree'Valid);
         return Degree;
      end Degree;


      procedure Set_Degree
        (Block  : in out Blocks.Base_Block_Type;
         Degree : in     Degree_Type)
      is
         procedure Write is new Blocks.Write_At(Degree_Type);
         Offset : constant Blocks.Base_Position_Type :=
            Size_Of_Booleans + Size_Of_Level;
         From   : constant Blocks.Base_Index_Type :=
            Blocks.Base_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Degree - 1, Degree);
         pragma Assert (Degree = Phys.Degree(Block));
      end Set_Degree;


      function Link
        (Block : Blocks.Base_Block_Type)
         return Address_Type
      is
         procedure Read is new Blocks.Read_At(Address_Type);
         Offset  : constant Blocks.Base_Position_Type :=
            Size_Of_Booleans + Size_Of_Level + Size_Of_Degree;
         From    : constant Blocks.Base_Index_Type :=
            Blocks.Base_Index_Type'First + Offset;
         Address : Address_Type;
      begin
         Read(Block, From, From + Size_Of_Address - 1, Address);
         return Address;
      end Link;


      procedure Set_Link
        (Block   : in out Blocks.Base_Block_Type;
         Address : in     Address_Type)
      is
         procedure Write is new Blocks.Write_At(Address_Type);
         Offset : constant Blocks.Base_Position_Type :=
            Size_Of_Booleans + Size_Of_Level + Size_Of_Degree;
         From   : constant Blocks.Base_Index_Type :=
            Blocks.Base_Index_Type'First + Offset;
      begin
         Write(Block, From, From + Size_Of_Address - 1, Address);
         pragma Assert (Link(Block) = Address);
      end Set_Link;


      procedure Init_Block
        (Block  : in out Blocks.Base_Block_Type;
         Is_Ok  : in     Boolean;
         Level  : in     Level_Type;
         Degree : in     Degree_Type;
         Link   : in     Address_Type)
      is
         procedure Set_Booleans
           (Block    : in out Blocks.Base_Block_Type;
            Booleans : in     Booleans_Type)
         is
            procedure Write is new Blocks.Write_At(Booleans_Type);
            Offset : constant Blocks.Base_Position_Type := 0;
            From   : constant Blocks.Base_Index_Type :=
               Blocks.Base_Index_Type'First + Offset;
         begin
            Write(Block, From, From + Size_Of_Booleans - 1, Booleans);
            pragma Assert (Phys.Is_Ok(Block) = Booleans.Is_Ok);
            pragma Assert (Phys.Has_High_Key(Block) = Booleans.Has_High_Key);
         end Set_Booleans;

         Booleans : constant Booleans_Type :=
            Booleans_Type'(Is_Ok        => Is_Ok,
                           Has_High_Key => False);
      begin
         Set_Booleans(Block, Booleans);
         Set_Level(Block, Level);
         Set_Degree(Block, Degree);
         Set_Link(Block, Link);
         pragma Assert (Phys.Level(Block) = Level);
         pragma Assert (Phys.Degree(Block) = Degree);
         pragma Assert (Phys.Has_High_Key(Block) = False);
         pragma Assert (Phys.Link(Block) = Link);
      end Init_Block;


      procedure Read_Key
        (Key_Context : in out Keys.Read_Context_Type;
         Block       : in     Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Keys.Key_Type;
         Success     :    out Boolean)
      is
         Cursor : Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Keys.Read(Key_Context, Block, Cursor, Key);
         Success := Blocks.Is_Valid(Cursor);
      end Read_Key;


      procedure Read_Child
        (Key_Context : in out Keys.Read_Context_Type;
         Block       : in     Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Child       :    out Valid_Address_Type;
         Success     :    out Boolean)
      is
         pragma Assert (Level(Block) > Leaf_Level);
         procedure Read_Child is new Blocks.Read(Valid_Address_Type);
         Cursor : Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Keys.Skip(Key_Context, Block, Cursor);
         Success := Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Read_Child(Block, Cursor, Child);
         Success := Blocks.Is_Valid(Cursor);
      end Read_Child;


      procedure Read_Value
        (Key_Context   : in out Keys.Read_Context_Type;
         Value_Context : in out Values.Read_Context_Type;
         Block         : in     Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Value         :    out Values.Value_Type;
         Success       :    out Boolean)
      is
         pragma Assert (Level(Block) = Leaf_Level);
         Cursor : Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Keys.Skip(Key_Context, Block, Cursor);
         Success := Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Values.Read(Value_Context, Block, Cursor, Value);
         Success := Blocks.Is_Valid(Cursor);
      end Read_Value;


      procedure Read_Entry
        (Key_Context : in out Keys.Read_Context_Type;
         Block       : in     Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         :    out Keys.Key_Type;
         Child       :    out Valid_Address_Type;
         Success     :    out Boolean)
      is
         pragma Assert (Level(Block) > Leaf_Level);
         procedure Read_Child is new Blocks.Read(Valid_Address_Type);
         Cursor : Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Keys.Read(Key_Context, Block, Cursor, Key);
         Success := Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Read_Child(Block, Cursor, Child);
         Success := Blocks.Is_Valid(Cursor);
      end Read_Entry;


      procedure Read_Entry
        (Key_Context   : in out Keys.Read_Context_Type;
         Value_Context : in out Values.Read_Context_Type;
         Block         : in     Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           :    out Keys.Key_Type;
         Value         :    out Values.Value_Type;
         Success       :    out Boolean)
      is
         pragma Assert (Level(Block) = Leaf_Level);
         Cursor : Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         Success := Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Keys.Read(Key_Context, Block, Cursor, Key);
         Success := Blocks.Is_Valid(Cursor);
         if not Success then
            return;
         end if;
         Values.Read(Value_Context, Block, Cursor, Value);
         Success := Blocks.Is_Valid(Cursor);
      end Read_Entry;


      procedure Read_High_Key
        (Key_Context : in out Keys.Read_Context_Type;
         Block       : in     Blocks.Base_Block_Type;
         High_Key    :    out Keys.Key_Type;
         Success     :    out Boolean)
      is
         pragma Assert (Degree(Block) = 0);
      begin
         if not Has_High_Key(Block) then
            Success := False;
            return;
         end if;
         declare
            Index  : constant Valid_Index_Type := 1;
            Cursor : Blocks.Cursor_Type := New_Cursor_From(Block, Index);
         begin
            Success := Blocks.Is_Valid(Cursor);
            if not Success then
               return;
            end if;
            Keys.Read(Key_Context, Block, Cursor, High_Key);
            Success := Blocks.Is_Valid(Cursor);
         end;
      end Read_High_Key;


      procedure Write_Entry
        (Key_Context : in out Keys.Write_Context_Type;
         Block       : in out Blocks.Base_Block_Type;
         Index       : in     Valid_Index_Type;
         Key         : in     Keys.Key_Type;
         Child       : in     Valid_Address_Type)
      is
         pragma Assert (Level(Block) > Leaf_Level);
         procedure Write_Child is new Blocks.Write(Valid_Address_Type);
         Cursor : Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         if not Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Keys.Write(Key_Context, Block, Cursor, Key);
         if not Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Write_Child(Block, Cursor, Child);
         if not Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         declare
            Size : constant Blocks.Base_Position_Type :=
               Blocks.Position(Cursor) - Entry_From_Pos(Block, Index);
         begin
            Set_Entry_Size(Block, Index, Size);
         end;
      end Write_Entry;


      procedure Write_Entry
        (Key_Context   : in out Keys.Write_Context_Type;
         Value_Context : in out Values.Write_Context_Type;
         Block         : in out Blocks.Base_Block_Type;
         Index         : in     Valid_Index_Type;
         Key           : in     Keys.Key_Type;
         Value         : in     Values.Value_Type)
      is
         pragma Assert (Level(Block) = Leaf_Level);
         Cursor : Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         if not Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Keys.Write(Key_Context, Block, Cursor, Key);
         if not Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Values.Write(Value_Context, Block, Cursor, Value);
         if not Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         declare
            Size : constant Blocks.Base_Position_Type :=
               Blocks.Position(Cursor) - Entry_From_Pos(Block, Index);
         begin
            Set_Entry_Size(Block, Index, Size);
         end;
      end Write_Entry;


      procedure Write_High_Key
        (Key_Context : in out Keys.Write_Context_Type;
         Block       : in out Blocks.Base_Block_Type;
         High_Key    : in     Keys.Key_Type)
      is
         pragma Assert (Degree(Block) = 0);
         pragma Assert (Has_High_Key(Block));
         Index  : constant Valid_Index_Type := 1;
         Cursor : Blocks.Cursor_Type := New_Cursor_From(Block, Index);
      begin
         if not Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         Keys.Write(Key_Context, Block, Cursor, High_Key);
         if not Blocks.Is_Valid(Cursor) then
            Phys.Set_Ok(Block, False);
            return;
         end if;
         declare
            Size : constant Blocks.Base_Position_Type :=
               Blocks.Position(Cursor) - Entry_From_Pos(Block, Index);
         begin
            Set_Entry_Size(Block, Index, Size);
         end;
      end Write_High_Key;

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
         Level   : Level_Type;
         Degree  : Degree_Type;
         Link    : Address_Type)
         return RW_Node_Type
   is
      Node : RW_Node_Type;
   begin
      Phys.Init_Block(Block   => Blocks.Base_Block_Type(Node),
                      Is_Ok   => Is_Ok,
                      Level   => Level,
                      Degree  => Degree,
                      Link    => Link);
      return Node;
   end New_RW_Node;


   function Invalid_Node
     return RW_Node_Type is
   begin
      return New_RW_Node(Is_Ok   => False,
                         Level   => Leaf_Level,
                         Degree  => 0,
                         Link    => Invalid_Address);
   end Invalid_Node;


   function Root_Node
     (Level : Level_Type)
      return RW_Node_Type is
   begin
      return New_RW_Node(Is_Ok  => True,
                         Level  => Level,
                         Degree => 0,
                         Link   => Invalid_Address);
   end Root_Node;


   function Is_Ok
     (Node : Node_Type)
      return Boolean is
   begin
      return Phys.Is_Ok(Blocks.Base_Block_Type(Node));
   end Is_Ok;


   function Level
     (Node : Node_Type)
      return Level_Type
   is
      pragma Assert (Is_Ok(Node));
   begin
      return Phys.Level(Blocks.Base_Block_Type(Node));
   end Level;


   function Degree
     (Node : Node_Type)
      return Degree_Type
   is
      pragma Assert (Is_Ok(Node));
   begin
      return Phys.Degree(Blocks.Base_Block_Type(Node));
   end Degree;


   function Is_Leaf
     (Node : Node_Type)
      return Boolean
   is
      pragma Assert (Is_Ok(Node));
   begin
      return Phys.Level(Blocks.Base_Block_Type(Node)) = Leaf_Level;
   end Is_Leaf;


   function Is_Inner
     (Node : Node_Type)
      return Boolean
   is
      pragma Assert (Is_Ok(Node));
   begin
      return not Is_Leaf(Node);
   end Is_Inner;


   function Link
     (Node : Node_Type)
      return Address_Type
   is
      pragma Assert (Is_Ok(Node));
   begin
      return Phys.Link(Blocks.Base_Block_Type(Node));
   end Link;


   function Valid_Link
     (Node : Node_Type)
      return Valid_Address_Type
   is
      pragma Assert (Is_Ok(Node));
   begin
      return To_Valid_Address(Phys.Link(Blocks.Base_Block_Type(Node)));
   end Valid_Link;


   procedure Get_High_Key
     (Node     : in  Node_Type;
      High_Key : out Keys.Key_Type;
      Success  : out Boolean) is
   begin
      if Degree(Node) = 0 then
         declare
            Key_Context : Keys.Read_Context_Type := Keys.New_Read_Context;
         begin
            Phys.Read_High_Key(Key_Context, Blocks.Base_Block_Type(Node),
                               High_Key, Success);
         end;
      else
         High_Key := Key(Node, Degree(Node));
         Success  := True;
      end if;
   end Get_High_Key;


   function Has_High_Key
     (Node : Nodes.Node_Type)
      return Boolean is
   begin
      if Degree(Node) = 0 then
         declare
            Key_Context  : Keys.Read_Context_Type := Keys.New_Read_Context;
            High_Key     : Keys.Key_Type;
            Has_High_Key : Boolean;
         begin
            Phys.Read_High_Key(Key_Context, Blocks.Base_Block_Type(Node),
                               High_Key, Has_High_Key);
            return Has_High_Key;
         end;
      else
         return True;
      end if;
   end Has_High_Key;


   function High_Key
     (Node : Nodes.Node_Type)
      return Keys.Key_Type
   is
      High_Key     : Keys.Key_Type;
      Has_High_Key : Boolean;
   begin
      Nodes.Get_High_Key(Node, High_Key, Has_High_Key);
      if not Has_High_Key then
         raise Tree_Error;
      end if;
      return High_Key;
   end High_Key;


   procedure Get_Key
     (Node        : in     Node_Type;
      Index       : in     Valid_Index_Type;
      Key         :    out Keys.Key_Type;
      Key_Context : in out Keys.Read_Context_Type)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Success : Boolean;
   begin
      Phys.Read_Key(Key_Context, Blocks.Base_Block_Type(Node), Index, Key,
                    Success);
      if not Success then
         raise Node_Error;
      end if;
   end Get_Key;


   function Key
     (Node  : Node_Type;
      Index : Valid_Index_Type)
      return Keys.Key_Type
   is
      Key         : Keys.Key_Type;
      Key_Context : Keys.Read_Context_Type := Keys.New_Read_Context;
   begin
      Get_Key(Node, Index, Key, Key_Context);
      return Key;
   end Key;


   procedure Get_Child
     (Node        : in     Node_Type;
      Index       : in     Valid_Index_Type;
      Child       :    out Valid_Address_Type;
      Key_Context : in out Keys.Read_Context_Type)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (Is_Inner(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Success : Boolean;
   begin
      Phys.Read_Child(Key_Context, Blocks.Base_Block_Type(Node), Index,
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
      Key_Context : Keys.Read_Context_Type := Keys.New_Read_Context;
   begin
      Get_Child(Node, Index, Child, Key_Context);
      return Child;
   end Child;


   procedure Get_Value
     (Node          : in     Node_Type;
      Index         : in     Valid_Index_Type;
      Value         :    out Values.Value_Type;
      Key_Context   : in out Keys.Read_Context_Type;
      Value_Context : in out Values.Read_Context_Type)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (Is_Leaf(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Success : Boolean;
   begin
      Phys.Read_Value(Key_Context, Value_Context,
                      Blocks.Base_Block_Type(Node), Index, Value, Success);
      if not Success then
         raise Node_Error;
      end if;
   end Get_Value;


   function Value
     (Node  : Node_Type;
      Index : Valid_Index_Type)
      return Values.Value_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (Is_Leaf(Node));
      pragma Assert (Index in 1 .. Degree(Node));

      Value         : Values.Value_Type;
      Key_Context   : Keys.Read_Context_Type   := Keys.New_Read_Context;
      Value_Context : Values.Read_Context_Type := Values.New_Read_Context;
   begin
      Get_Value(Node, Index, Value, Key_Context, Value_Context);
      return Value;
   end Value;


   function Key_Position
     (Node : Node_Type;
      Key  : Keys.Key_Type)
      return Index_Type
   is
      Key_Context : Keys.Read_Context_Type := Keys.New_Read_Context;

      function Get_Key
        (Node  : Node_Type;
         Index : Index_Type)
         return Keys.Key_Type
      is
         Key : Keys.Key_Type;
      begin
         Get_Key(Node, Index, Key, Key_Context);
         return Key;
      end Get_Key;


      function Key_Position_Uniform_Binary
        (Node : Node_Type;
         Key  : Keys.Key_Type)
         return Index_Type
      is
         pragma Inline (Key_Position_Uniform_Binary);
         pragma Assert (Is_Ok(Node));
      begin
         if Degree(Node) > 0 then
            declare
               procedure Find is
                  new Utils.Binary_Search.Uniform_Find_Best_In_Container
                        (Container_Type      => Node_Type,
                         Extended_Index_Type => Index_Type,
                         Invalid_Index       => Invalid_Index,
                         Item_Type           => Keys.Key_Type,
                         Get                 => Get_Key,
                         Compare             => Keys.Compare);
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
         Key  : Keys.Key_Type)
         return Index_Type
      is
         pragma Inline (Key_Position_Binary);
         pragma Assert (Is_Ok(Node));
      begin
         if Degree(Node) > 0 then
            declare
               procedure Find is
                  new Utils.Binary_Search.Find_Best_In_Container
                        (Container_Type => Node_Type,
                         Index_Type     => Valid_Index_Type,
                         Item_Type      => Keys.Key_Type,
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
         Key  : Keys.Key_Type)
         return Index_Type
      is
         pragma Inline (Key_Position_Linear);
         pragma Assert (Is_Ok(Node));
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
      return Index_Type
   is
      pragma Assert (Is_Ok(Node));
   begin
      for I in 1 .. Degree(Node) loop
         if Child = Nodes.Child(Node, I) then
            return I;
         end if;
      end loop;
      return Invalid_Index;
   end Child_Position;


   ----------
   -- Node operations.

   procedure Set_Link
     (Node     : in out RW_Node_Type;
      Neighbor : in     Address_Type)
   is
      pragma Assert (Is_Ok(Node));
   begin
      Phys.Set_Link(Blocks.Base_Block_Type(Node), Neighbor);
   end Set_Link;


   procedure Set_Link
     (Node     : in out RW_Node_Type;
      Neighbor : in     Valid_Address_Type)
   is
      pragma Assert (Is_Ok(Node));
   begin
      Phys.Set_Link(Blocks.Base_Block_Type(Node), To_Address(Neighbor));
   end Set_Link;


   function Split_Position
     (Node : RW_Node_Type)
      return Valid_Index_Type
   is
      use type Blocks.Base_Position_Type;

      pragma Assert (Is_Ok(Node));

      Total    : constant Blocks.Base_Position_Type :=
         Phys.Entries_Size(Blocks.Base_Block_Type(Node));
      Sum      : Blocks.Base_Position_Type := 0;
      Prev_Min : Blocks.Base_Position_Type := 0;
   begin
      for I in 1 .. Degree(Node) loop
         Sum := Sum + Phys.Entry_Size(Blocks.Base_Block_Type(Node), I);
         declare
            This_Min : constant Blocks.Base_Position_Type :=
               Blocks.Base_Position_Type'Min(Sum, Total - Sum);
         begin
            if Prev_Min > This_Min then
               return I;
            end if;
            Prev_Min := This_Min;
         end;
      end loop;
      raise Tree_Error; -- never reached
   end Split_Position;


   procedure Copy_Entry
     (Node              : in out RW_Node_Type;
      Source            : in     RW_Node_Type;
      Index             : in     Valid_Index_Type;
      Key_Read_Context  : in out Keys.Read_Context_Type;
      Key_Write_Context : in out Keys.Write_Context_Type;
      Shift_By          : in     Integer := 0)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (Is_Inner(Node));

      New_Index : constant Valid_Index_Type :=
         Valid_Index_Type(Integer(Index) + Shift_By);
      Key       : Keys.Key_Type;
      Child     : Valid_Address_Type;
      Success   : Boolean;
   begin
      Phys.Read_Entry(Key_Read_Context, Blocks.Base_Block_Type(Source),
                      Index, Key, Child, Success);
      if not Success then
         raise Node_Error;
      end if;
      pragma Assert (Is_Valid(Child));
      Phys.Write_Entry(Key_Write_Context, Blocks.Base_Block_Type(Node),
                       New_Index, Key, Child);
   end Copy_Entry;


   procedure Copy_Entry
     (Node                : in out RW_Node_Type;
      Source              : in     RW_Node_Type;
      Index               : in     Valid_Index_Type;
      Key_Read_Context    : in out Keys.Read_Context_Type;
      Key_Write_Context   : in out Keys.Write_Context_Type;
      Value_Read_Context  : in out Values.Read_Context_Type;
      Value_Write_Context : in out Values.Write_Context_Type;
      Shift_By            : in     Integer := 0)
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (Is_Leaf(Node));

      New_Index : constant Valid_Index_Type :=
         Valid_Index_Type(Integer(Index) + Shift_By);
      Key       : Keys.Key_Type;
      Value     : Values.Value_Type;
      Success   : Boolean;
   begin
      Phys.Read_Entry(Key_Read_Context, Value_Read_Context,
                      Blocks.Base_Block_Type(Source), Index, Key, Value,
                      Success);
      if not Success then
         raise Node_Error;
      end if;
      Phys.Write_Entry(Key_Write_Context, Value_Write_Context,
                       Blocks.Base_Block_Type(Node), New_Index, Key, Value);
   end Copy_Entry;


   function Insertion
     (Node  : RW_Node_Type;
      Index : Valid_Index_Type;
      Key   : Keys.Key_Type;
      Child : Valid_Address_Type)
      return RW_Node_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (Is_Inner(Node));

      N : RW_Node_Type := New_RW_Node(Is_Ok  => True,
                                      Level  => Level(Node),
                                      Degree => Degree(Node) + 1,
                                      Link   => Link(Node));
      Key_Read_Context  : Keys.Read_Context_Type := Keys.New_Read_Context;
      Key_Write_Context : Keys.Write_Context_Type := Keys.New_Write_Context;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, Blocks.Base_Block_Type(N), Index,
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
      Key   : Keys.Key_Type;
      Value : Values.Value_Type)
      return RW_Node_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (Is_Leaf(Node));

      N : RW_Node_Type := New_RW_Node(Is_Ok  => True,
                                      Level  => Level(Node),
                                      Degree => Degree(Node) + 1,
                                      Link   => Link(Node));
      Key_Read_Context    : Keys.Read_Context_Type := Keys.New_Read_Context;
      Key_Write_Context   : Keys.Write_Context_Type := Keys.New_Write_Context;
      Value_Read_Context  : Values.Read_Context_Type := Values.New_Read_Context;
      Value_Write_Context : Values.Write_Context_Type :=
         Values.New_Write_Context;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                    Value_Read_Context, Value_Write_Context);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, Value_Write_Context,
                       Blocks.Base_Block_Type(N), Index, Key, Value);
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
      Key   : Keys.Key_Type;
      Child : Valid_Address_Type)
      return RW_Node_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (Is_Inner(Node));
      pragma Assert (Is_Valid(Child));

      N : RW_Node_Type := New_RW_Node(Is_Ok  => True,
                                      Level  => Level(Node),
                                      Degree => Degree(Node),
                                      Link   => Link(Node));
      Key_Read_Context  : Keys.Read_Context_Type  := Keys.New_Read_Context;
      Key_Write_Context : Keys.Write_Context_Type := Keys.New_Write_Context;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, Blocks.Base_Block_Type(N), Index,
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
      Key   : Keys.Key_Type;
      Value : Values.Value_Type)
      return RW_Node_Type
   is
      pragma Assert (Is_Ok(Node));
      pragma Assert (Is_Leaf(Node));

      N : RW_Node_Type := New_RW_Node(Is_Ok  => True,
                                      Level  => Level(Node),
                                      Degree => Degree(Node),
                                      Link   => Link(Node));
      Key_Read_Context    : Keys.Read_Context_Type := Keys.New_Read_Context;
      Key_Write_Context   : Keys.Write_Context_Type := Keys.New_Write_Context;
      Value_Read_Context  : Values.Read_Context_Type := Values.New_Read_Context;
      Value_Write_Context : Values.Write_Context_Type :=
         Values.New_Write_Context;
   begin
      for I in 1 .. Index - 1 loop
         Copy_Entry(N, Node, I, Key_Read_Context, Key_Write_Context,
                    Value_Read_Context, Value_Write_Context);
         if not Is_Ok(N) then
            return N;
         end if;
      end loop;

      Phys.Write_Entry(Key_Write_Context, Value_Write_Context,
                       Blocks.Base_Block_Type(N), Index, Key, Value);
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

      N : RW_Node_Type := New_RW_Node(Is_Ok  => True,
                                      Level  => Level(Node),
                                      Degree => Degree(Node) - 1,
                                      Link   => Link(Node));
   begin
      if Is_Inner(Node) then
         declare
            Key_Read_Context  : Keys.Read_Context_Type := Keys.New_Read_Context;
            Key_Write_Context : Keys.Write_Context_Type :=
               Keys.New_Write_Context;
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
            Key_Read_Context    : Keys.Read_Context_Type :=
               Keys.New_Read_Context;
            Key_Write_Context   : Keys.Write_Context_Type :=
               Keys.New_Write_Context;
            Value_Read_Context  : Values.Read_Context_Type :=
               Values.New_Read_Context;
            Value_Write_Context : Values.Write_Context_Type :=
               Values.New_Write_Context;
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
      if Degree(N) = 0 and Degree(Node) = 1 then
         declare
            High_Key    : constant Keys.Key_Type := Key(Node, 1);
            Key_Context : Keys.Write_Context_Type := Keys.New_Write_Context;
         begin
            Phys.Set_Has_High_Key(Blocks.Base_Block_Type(N), True);
            Phys.Write_High_Key(Key_Context, Blocks.Base_Block_Type(N),
                                High_Key);
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
         N : RW_Node_Type := New_RW_Node(Is_Ok  => True,
                                         Level  => Level(Node),
                                         Degree => To - From + 1,
                                         Link   => Link(Node));
         Shift_By : constant Integer := -1 * Integer(From) + 1;
      begin
         if Is_Inner(Node) then
            declare
               Key_Read_Context  : Keys.Read_Context_Type :=
                  Keys.New_Read_Context;
               Key_Write_Context : Keys.Write_Context_Type :=
                  Keys.New_Write_Context;
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
               Key_Read_Context  : Keys.Read_Context_Type :=
                  Keys.New_Read_Context;
               Key_Write_Context : Keys.Write_Context_Type :=
                  Keys.New_Write_Context;
               Value_Read_Context  : Values.Read_Context_Type :=
                  Values.New_Read_Context;
               Value_Write_Context : Values.Write_Context_Type :=
                  Values.New_Write_Context;
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


   function Combination
     (Left_Node  : RW_Node_Type;
      Right_Node : RW_Node_Type)
      return RW_Node_Type is
   begin
      if not Is_Ok(Left_Node) or not Is_Ok(Right_Node) then
         return Invalid_Node;
      end if;

      if Degree(Left_Node) = 0 and Degree(Right_Node) = 0 then
         if Phys.Has_High_Key(Blocks.Base_Block_Type(Right_Node)) then
            return Right_Node;
         else
            return Left_Node;
         end if;
      end if;

      declare
         pragma Assert (Is_Ok(Left_Node));
         pragma Assert (Is_Ok(Right_Node));
         pragma Assert (Validation(Left_Node) /= Too_Large or
                        Validation(Right_Node) /= Too_Large);
         pragma Assert (Is_Leaf(Left_Node) = Is_Leaf(Right_Node));
         pragma Assert (Level(Left_Node) = Level(Right_Node));

         Left_Degree  : constant Degree_Type := Degree(Left_Node);
         Right_Degree : constant Degree_Type := Degree(Right_Node);
         Degree       : constant Degree_Type := Left_Degree + Right_Degree;
         N : RW_Node_Type := New_RW_Node(Is_Ok  => True,
                                         Level  => Level(Right_Node),
                                         Degree => Degree,
                                         Link   => Link(Right_Node));
      begin
         if Is_Inner(Right_Node) then
            declare
               Key_Read_Context  : Keys.Read_Context_Type :=
                  Keys.New_Read_Context;
               Key_Write_Context : Keys.Write_Context_Type :=
                  Keys.New_Write_Context;
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
               Key_Read_Context  : Keys.Read_Context_Type :=
                  Keys.New_Read_Context;
               Key_Write_Context : Keys.Write_Context_Type :=
                  Keys.New_Write_Context;
               Value_Read_Context  : Values.Read_Context_Type :=
                  Values.New_Read_Context;
               Value_Write_Context : Values.Write_Context_Type :=
                  Values.New_Write_Context;
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
      return Blocks.Size_Type
   is
      pragma Assert (Nodes.Is_Ok(Node));
      Last_Used : constant Blocks.Base_Position_Type :=
         Phys.Total_Size(Blocks.Base_Block_Type(Node));
   begin
      return Blocks.Size_Type(Last_Used);
   end Size_Of;


   function Is_Safe
     (Node    : Node_Type;
      Is_Root : Boolean := False)
      return Boolean is
   begin
      return Validation(Node, Is_Root) = Valid;
   end Is_Safe;


   function Validation
     (Node    : Node_Type;
      Is_Root : Boolean := False)
      return Validation_State_Type
   is
      use type Blocks.Size_Type;
   begin
      if not Nodes.Is_Ok(Node) then
         return Too_Large;
      end if;

      declare
         use type Blocks.Base_Position_Type;
         Min_Degree : Degree_Type;
         Max_Space  : constant Blocks.Position_Type :=
            Phys.Effective_Block_Space;
         Needed     : constant Blocks.Base_Position_Type :=
            Phys.Entries_Size(Blocks.Base_Block_Type(Node));
      begin
         if Is_Root or Is_Leaf(Node) then
            Min_Degree := 0;
         else
            Min_Degree := 2;
         end if;

         if Degree(Node) < Min_Degree then
            return Too_Small;
         elsif Needed > Max_Space then
            return Too_Large;
         else
            return Valid;
         end if;
      end;
   end Validation;


   function Max_Key_Size
     (Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type
   is
      use type Blocks.Base_Position_Type;
      Max_Entry_Size        : constant Blocks.Base_Position_Type :=
         Phys.Effective_Block_Space * 1 / 4;
      Max_Value_Size_As_Pos : constant Blocks.Position_Type :=
         Blocks.Base_Position_Type(Max_Value_Size);
   begin
      return Blocks.Size_Type
               (Phys.Max_Key_Size(Max_Entry_Size => Max_Entry_Size,
                                  Max_Value_Size => Max_Value_Size_As_Pos));
   end Max_Key_Size;


   function To_Block
     (Node : Node_Type)
      return Blocks.Block_Type
   is
      pragma Assert (Nodes.Is_Ok(Node));
      use type Blocks.Base_Position_Type;
   begin
      pragma Assert (Phys.Total_Size(Blocks.Base_Block_Type(Node)) <=
                     Blocks.Block_Size);
      pragma Assert (Validation(Node) = Valid);
      return Blocks.To_Block
               (Block     => Blocks.Base_Block_Type(Node),
                Last_Used => Phys.Total_Size(Blocks.Base_Block_Type(Node)));
   end To_Block;

end Nodes;

