with Tree.To_Strings;

with DB.Types.Byte_Arrays.Bounded;
with DB.Types.Byte_Arrays.Unbounded;
with DB.Types.Keys;
with DB.Types.Values;

private
package Tree.Types is
   pragma Elaborate_Body;

   type Count_Type is mod 2**64;
   type Generator_Type is (Pseudo_Random_Gen, URL_Gen);

   ----------
   -- Key_Type

   subtype Key_Type is DB.Types.Keys.Key_Type;

   function "=" (Left, Right : Key_Type) return Boolean
   renames DB.Types.Keys."=";

   function "<=" (Left, Right : Key_Type) return Boolean
   renames DB.Types.Keys."<=";

   function To_String (K : Key_Type) return String
   renames To_Strings.To_String;

   ----------
   -- Value_Type

   subtype Value_Type is DB.Types.Values.Value_Type;

   function New_Value (S : String) return Value_Type;

   function New_Value
     (S : DB.Types.Byte_Arrays.Indefinite_Buffer_Type)
      return Value_Type;

   function From_Bounded (S : DB.Types.Byte_Arrays.Bounded.String_Type)
      return Value_Type;
   function From_Unbounded (S: DB.Types.Byte_Arrays.Unbounded.String_Type)
      return Value_Type;

   function To_Bounded (V : Value_Type)
      return DB.Types.Byte_Arrays.Bounded.String_Type;
   function To_Unbounded (V : Value_Type)
      return DB.Types.Byte_Arrays.Unbounded.String_Type;

   function Null_Value return Value_Type;

   function To_String (V : DB.Types.Values.Value_Type) return String;

   ----------
   -- Pair or key and value.

   type Key_Value_Type is
      record
         Key   : DB.Types.Keys.Key_Type;
         Value : Value_Type;
      end record;
   type Key_Value_Array_Type is array (Positive range <>) of Key_Value_Type;
   type Key_Value_Array_Access_Type is access Key_Value_Array_Type;
   subtype Char_Type is Character;

   function Key (KV : Key_Value_Type) return DB.Types.Keys.Key_Type;
   function Value (KV : Key_Value_Type) return Value_Type;

private

end Tree.Types;

