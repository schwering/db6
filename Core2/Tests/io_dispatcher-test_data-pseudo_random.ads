with DB.Blocks;
with DB.Types.Keys;
with DB.Types.Values.Bounded;

private
package IO_Dispatcher.Test_Data.Pseudo_Random is

   procedure Init_Key_Value_Pairs (Init : in Count_Type);
   procedure Reset_String_Generation;
   function Random_Entry return Key_Value_Type;
   procedure Finalize_Key_Value_Pairs;

private
   Initial_KV : Count_Type;
   Current_KV : Count_Type := 1;

   Key_Value_Pairs : Key_Value_Array_Access_Type
                   := new Key_Value_Array_Type(1 .. 10_000);

end IO_Dispatcher.Test_Data.Pseudo_Random;

