with IO_Dispatcher.Test_Data.Pseudo_Random;
with IO_Dispatcher.Test_Data.URLs;

package body IO_Dispatcher.Test_Data is

   function Key (KV : Key_Value_Type) return DB.Types.Keys.Key_Type is
   begin
      return KV.Key;
   end;


   function Value (KV : Key_Value_Type)
      return Values.String_Type is
   begin
      return KV.Value;
   end;


   procedure Init_Key_Value_Pairs
     (Generator : in Generator_Type;
      Count     : in Count_Type) is
   begin
      Test_Data.Generator := Generator;
      case Generator is
         when Pseudo_Random_Gen =>
            Test_Data.Pseudo_Random.Init_Key_Value_Pairs(Count);
         when URL_Gen =>
            Test_Data.URLs.Init_Key_Value_Pairs(Count);
      end case;
   end;


   procedure Reset_String_Generation is
   begin
      case Generator is
         when Pseudo_Random_Gen =>
            Test_Data.Pseudo_Random.Reset_String_Generation;
         when URL_Gen =>
            Test_Data.URLs.Reset_String_Generation;
      end case;
   end;


   function Random_Entry return Key_Value_Type is
   begin
      case Generator is
         when Pseudo_Random_Gen =>
            return Test_Data.Pseudo_Random.Random_Entry;
         when URL_Gen =>
            return Test_Data.URLs.Random_Entry;
      end case;
   end;


   procedure Finalize_Key_Value_Pairs is
   begin
      case Generator is
         when Pseudo_Random_Gen =>
            Test_Data.Pseudo_Random.Finalize_Key_Value_Pairs;
         when URL_Gen =>
            Test_Data.URLs.Finalize_Key_Value_Pairs;
      end case;
   end;

end IO_Dispatcher.Test_Data;

