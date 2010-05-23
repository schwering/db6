with DB.Blocks;
with DB.Types.Keys;
with DB.Types.Values.Bounded;

private
package Tree.Test_Data is

   Max_Key_Size : constant := (DB.Blocks.Block_Size - 9) * 1/4 - 2 - 4 - 4
   --                                                 ^M     ^B  ^P  ^VL ^VB
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

   package Keys    renames DB.Types.Keys;
   package Rows    renames DB.Types.Keys.Rows;
   package Columns renames DB.Types.Keys.Columns;
   package Values  renames DB.Types.Values.Bounded;

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


   type Generator_Type is (Pseudo_Random_Gen, URL_Gen);

   Generator  : Generator_Type;

   procedure Init_Key_Value_Pairs
     (Generator : in Generator_Type;
      Count     : in Count_Type);
   procedure Reset_String_Generation;
   function Random_Entry return Key_Value_Type;
   procedure Finalize_Key_Value_Pairs;

end Tree.Test_Data;

