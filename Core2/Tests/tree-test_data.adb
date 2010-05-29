with Tree.Test_Data.Pseudo_Random;
with Tree.Test_Data.URLs;

package body Tree.Test_Data is

   procedure Init_Key_Value_Pairs
     (Generator : in Types.Generator_Type;
      Count     : in Types.Count_Type) is
   begin
      Test_Data.Generator := Generator;
      case Generator is
         when Types.Pseudo_Random_Gen =>
            Test_Data.Pseudo_Random.Init_Key_Value_Pairs(Count);
         when Types.URL_Gen =>
            Test_Data.URLs.Init_Key_Value_Pairs(Count);
      end case;
   end;


   procedure Reset_String_Generation is
   begin
      case Generator is
         when Types.Pseudo_Random_Gen =>
            Test_Data.Pseudo_Random.Reset_String_Generation;
         when Types.URL_Gen =>
            Test_Data.URLs.Reset_String_Generation;
      end case;
   end;


   function Random_Entry return Types.Key_Value_Type is
   begin
      case Generator is
         when Types.Pseudo_Random_Gen =>
            return Test_Data.Pseudo_Random.Random_Entry;
         when Types.URL_Gen =>
            return Test_Data.URLs.Random_Entry;
      end case;
   end;


   procedure Finalize_Key_Value_Pairs is
   begin
      case Generator is
         when Types.Pseudo_Random_Gen =>
            Test_Data.Pseudo_Random.Finalize_Key_Value_Pairs;
         when Types.URL_Gen =>
            Test_Data.URLs.Finalize_Key_Value_Pairs;
      end case;
   end;

end Tree.Test_Data;

