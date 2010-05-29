private
package Tree.Test_Data.Pseudo_Random is

   procedure Init_Key_Value_Pairs (Init : in Types.Count_Type);
   procedure Reset_String_Generation;
   function Random_Entry return Types.Key_Value_Type;
   procedure Finalize_Key_Value_Pairs;

private
   Initial_KV : Types.Count_Type;
   Current_KV : Types.Count_Type := 1;

   Key_Value_Pairs : Types.Key_Value_Array_Access_Type :=
      new Types.Key_Value_Array_Type(1 .. 10_000);

end Tree.Test_Data.Pseudo_Random;

