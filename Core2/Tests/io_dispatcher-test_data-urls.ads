private
package IO_Dispatcher.Test_Data.URLs is

   procedure Init_Key_Value_Pairs (Init : in Count_Type);
   procedure Reset_String_Generation;
   function Random_Entry return Key_Value_Type;
   procedure Finalize_Key_Value_Pairs is null;

private
   subtype Index_Type is Count_Type range 0 .. Count_Type'Last;
   Initial_KV : Index_Type;
   Current_KV : Index_Type := 1;

end IO_Dispatcher.Test_Data.URLs;

