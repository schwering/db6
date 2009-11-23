with DB.BTrees;
with DB.Types.Keys;

private
package IO_Dispatcher.Random is

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

