with DB.IO.Blocks;
with DB.BTrees;
with DB.Types.Keys;

private
package IO_Dispatcher.Random is

   Block_Size : constant := DB.IO.Blocks.Block_Size;
   Max_Key_Size : constant := (Block_Size - 16) * 1/4 - 2 - 4 - 4
   --                                       ^M     ^B  ^P  ^VL ^VB
                                ;--+ 1; -- to enforce heaped map
   Max_Value_Size : constant := 4 + 4;
   --                           ^VL ^VB

   Max_String_Length : constant := Max_Key_Size - 4 - 8;
   --                                             ^KL ^KT

   -- M = Meta_Data_Size
   -- B = BTree Req. (1): entries <= 1/4*Block_Size
   -- P = Long_Position_Type for inner nodes
   -- KL = Value_Length_Size for key
   -- KT = Timestamp_Size (part of Key_Type)
   -- VL = Value_Length_Size for value
   -- VB = Value_Buffer_Size for value

   package Keys    renames DB.BTrees.Keys;
   package Rows    renames DB.BTrees.Keys.Rows;
   package Columns renames DB.BTrees.Keys.Columns;
   package Values  renames DB.BTrees.Values;

   type Count_Type is mod 2**64;
   type Key_Value_Type is
      record
         Key   : DB.Types.Keys.Key_Type;
         Value : Values.String_Type;
      end record;
   type Key_Value_Array_Type is array (Positive range <>) of Key_Value_Type;
   type Key_Value_Array_Access_Type is access Key_Value_Array_Type;
   subtype Char_Type is Character;

   function Key (KV : Key_Value_Type) return DB.Types.Keys.Key_Type;
   function Value (KV : Key_Value_Type) return Values.String_Type;

   Initial_KV : Count_Type;
   Current_KV : Count_Type := 1;

   procedure Init_Key_Value_Pairs (Init : in Count_Type);
   procedure Reset_String_Generation;
   function Random_Entry return Key_Value_Type;
   function Some_Entry return Key_Value_Type;
   procedure Finalize_Key_Value_Pairs;

private
   Key_Value_Pairs : Key_Value_Array_Access_Type
                   := new Key_Value_Array_Type(1 .. 10_000);

end IO_Dispatcher.Random;

