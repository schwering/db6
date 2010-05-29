with DB.Blocks;
with Tree.Types;

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

   Generator : Types.Generator_Type;

   procedure Init_Key_Value_Pairs
     (Generator : in Types.Generator_Type;
      Count     : in Types.Count_Type);
   procedure Reset_String_Generation;
   function Random_Entry return Types.Key_Value_Type;
   procedure Finalize_Key_Value_Pairs;

end Tree.Test_Data;

