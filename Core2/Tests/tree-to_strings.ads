with DB.Types;
with DB.Types.Times;
with DB.Types.Keys;
with DB.Types.Strings;
with DB.Types.Strings.Bounded;
with DB.Types.Strings.Unbounded;
with DB.Types.Byte_Arrays;
with DB.Types.Byte_Arrays.Bounded;
with DB.Types.Byte_Arrays.Unbounded;
with DB.Blocks.Local_IO;
with DB.Blocks.Memory_IO;

private
package Tree.To_Strings is

   function To_String (S : DB.Types.Strings.Bounded.String_Type)
      return String;

   function To_String (S : DB.Types.Strings.Unbounded.String_Type)
      return String;

   function To_String (Time : DB.Types.Times.Time_Type)
      return String;

   function To_String (Key : DB.Types.Keys.Key_Type)
      return String;

   function To_String (Value : DB.Types.Byte_Arrays.Bounded.String_Type)
      return String;

   function To_String (Value : DB.Types.Byte_Arrays.Unbounded.String_Type)
      return String;

   function To_String (Address : DB.Blocks.Local_IO.Address_Type)
      return String;

   function To_String (Address : DB.Blocks.Memory_IO.Address_Type)
      return String;

end Tree.To_Strings;

