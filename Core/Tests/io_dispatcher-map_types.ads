with IO_Dispatcher.Random;
with IO_Dispatcher.To_Strings;

with DB.Tables;

with DB.Types.Keys;
with DB.Types.Values.Bounded;
with DB.Types.Values.Unbounded;
with DB.Types.Times;

private
package IO_Dispatcher.Map_Types is

   Max_Key_Size   : constant := 2 + 1000 + 8 
                                + 1; -- to enforce heaped map
   Max_Value_Size : constant := 8;

   ----------
   -- Key_Type

   subtype Key_Type is DB.Types.Keys.Key_Type;

   function To_String (K : Key_Type) return String
   renames To_Strings.To_String;

   ----------
   -- Value_Type

   type Value_Type is new DB.Tables.Value_Type with private;

   overriding function From_Bounded (S : DB.Types.Values.Bounded.String_Type)
      return Value_Type;
   overriding function From_Unbounded (S: DB.Types.Values.Unbounded.String_Type)
      return Value_Type;

   overriding function To_Bounded (V : Value_Type)
      return DB.Types.Values.Bounded.String_Type;
   overriding function To_Unbounded (V : Value_Type)
      return DB.Types.Values.Unbounded.String_Type;

   overriding function "=" (Left, Right : Value_Type) return Boolean;

   function Null_Value return DB.Tables.Value_Type'Class;

   function To_String (V : DB.Tables.Value_Type'Class) return String;

   ----------
   -- Interface with Random.

   function Get_Value
     (KV : Random.Key_Value_Type)
      return DB.Tables.Value_Type'Class;

private
   package Values_Impl renames DB.Types.Values.Unbounded;

   type Value_Type is new DB.Tables.Value_Type with
      record
         S : Values_Impl.String_Type := Values_Impl.Empty_String;
      end record;

end IO_Dispatcher.Map_Types;

