with Tree.To_Strings;

with DB.Maps;

with DB.Types.Keys;
with DB.Types.Values.Bounded;
with DB.Types.Values.Bounded.Streams;
with DB.Types.Values.Unbounded;

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

   function Short_Bound (Left : Key_Type) return Key_Type
   renames DB.Types.Keys.Short_Bound;

   function Short_Delimiter (Left, Right : Key_Type) return Key_Type
   renames DB.Types.Keys.Short_Delimiter;

   function To_String (K : Key_Type) return String
   renames To_Strings.To_String;

   ----------
   -- Value_Type

   type Value_Type is new DB.Maps.Value_Type with private;

   function New_Value
     (S : DB.Types.Values.Indefinite_Buffer_Type)
      return Value_Type;

   function From_Bounded (S : DB.Types.Values.Bounded.String_Type)
      return Value_Type;
   function From_Unbounded (S: DB.Types.Values.Unbounded.String_Type)
      return Value_Type;

   function To_Bounded (V : Value_Type)
      return DB.Types.Values.Bounded.String_Type;
   function To_Unbounded (V : Value_Type)
      return DB.Types.Values.Unbounded.String_Type;

   overriding
   procedure Write
     (Stream : in out DB.Types.Values.Bounded.Streams.Stream_Type'Class;
      Value  : in     Value_Type);

   overriding
   procedure Read
     (Stream : in out DB.Types.Values.Bounded.Streams.Stream_Type'Class;
      Value  :    out Value_Type);

   overriding function Equals
     (Left  : Value_Type;
      Right : Value_Type)
      return Boolean;

   overriding function Image (V : Value_Type) return String;

   function Null_Value return Value_Type;

   function To_String (V : DB.Maps.Value_Type'Class) return String;

   ----------
   -- Pair or key and value.

   type Key_Value_Type is
      record
         Key   : DB.Types.Keys.Key_Type;
         Value : Types.Value_Type;
      end record;
   type Key_Value_Array_Type is array (Positive range <>) of Key_Value_Type;
   type Key_Value_Array_Access_Type is access Key_Value_Array_Type;
   subtype Char_Type is Character;

   function Key (KV : Key_Value_Type) return DB.Types.Keys.Key_Type;
   function Value (KV : Key_Value_Type) return Value_Type;

private
   package Values_Impl renames DB.Types.Values.Unbounded;

   type Value_Type is new DB.Maps.Value_Type with
      record
         S : Values_Impl.String_Type := Values_Impl.Empty_String;
      end record;

end Tree.Types;

