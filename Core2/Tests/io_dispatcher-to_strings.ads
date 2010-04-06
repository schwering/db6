with DB.Types;
with DB.Types.Times;
with DB.Types.Keys;
with DB.Types.Strings;
with DB.Types.Strings.Bounded;
with DB.Types.Strings.Unbounded;
with DB.Types.Values;
with DB.Types.Values.Bounded;
with DB.Types.Values.Unbounded;
with DB.Blocks.Local_IO;
with DB.Blocks.Memory_IO;

private
package IO_Dispatcher.To_Strings is

   function To_String (S : DB.Types.Strings.Bounded.String_Type)
      return String;

   function To_String (S : DB.Types.Strings.Unbounded.String_Type)
      return String;

   function To_String (Time : DB.Types.Times.Number_Type)
      return String;

   function To_String (Key : DB.Types.Keys.Key_Type)
      return String;

   function To_String (Value : DB.Types.Values.Bounded.String_Type)
      return String;

   function To_String (Value : DB.Types.Values.Unbounded.String_Type)
      return String;

   function To_String (Address : DB.Blocks.Local_IO.Address_Type)
      return String;

   function To_String (Address : DB.Blocks.Memory_IO.Address_Type)
      return String;

end IO_Dispatcher.To_Strings;

