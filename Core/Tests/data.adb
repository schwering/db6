with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Aux; use Ada.Numerics.Aux;

with Random; use Random;

with DB.IO.Blocks;
with DB.Types;
with DB.Types.Keys;
with DB.Types.Values;

procedure Data
is
   use type DB.IO.Blocks.Size_Type;
   Count : constant Count_Type := 10_000_000;
   Counter         : Count_Type := 0;
   Key_Average     : Double := 0.0;
   Key_Squared     : Double := 0.0;
   Key_Variance    : Double := 0.0;
   Key_Deviation   : Double := 0.0;
   Value_Average   : Double := 0.0;
   Value_Squared   : Double := 0.0;
   Value_Variance  : Double := 0.0;
   Value_Deviation : Double := 0.0;
begin
   Init_Key_Value_Pairs(10*Count+1);
   for I in 1 .. Count loop
      declare
         KV            : constant Key_Value_Type := Random_Entry;
         Key_Size      : constant DB.IO.Blocks.Size_Type
                       := DB.Types.Keys.Size_Of(KV.Key);
         Value_Size    : constant DB.IO.Blocks.Size_Type
                       := DB.Types.Values.Size_Of(KV.Value);
      begin
         Key_Average := Double(Counter) / Double(Counter+1) * Key_Average
                      + Double(Key_Size) / Double(Counter+1);
         Key_Squared := Double(Counter) / Double(Counter+1) * Key_Squared
                      + Double(Key_Size*Key_Size) / Double(Counter+1);
         Value_Average := Double(Counter) / Double(Counter+1) * Value_Average
                        + Double(Value_Size) / Double(Counter+1);
         Value_Squared := Double(Counter) / Double(Counter+1) * Value_Squared
                        + Double(Value_Size*Value_Size) / Double(Counter+1);
         Counter := Counter + 1;
      end;
   end loop;
   Key_Variance    := Key_Squared - Key_Average*Key_Average;
   Key_Deviation   := Sqrt(Key_Variance);
   Value_Variance  := Value_Squared - Value_Average*Value_Average;
   Value_Deviation := Sqrt(Value_Variance);
   Put_Line("Key Average:     " & Natural'Image(Natural(Key_Average)));
   Put_Line("Key Deviation:   " & Natural'Image(Natural(Key_Deviation)));
   New_Line;
   Put_Line("Value Variance:  " & Natural'Image(Natural(Value_Average)));
   Put_Line("Value Deviation: " & Natural'Image(Natural(Value_Deviation)));
end Data;

