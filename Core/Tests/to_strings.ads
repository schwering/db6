-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with DB.Types;
with DB.Types.Columns;
with DB.Types.Rows;
with DB.Types.Times;
with DB.Types.Keys;
with DB.Types.Values;
with DB.IO.Blocks.File_IO;
with DB.IO.Blocks.Direct_IO;
with DB.IO.Blocks.Device_IO;
with DB.IO.Blocks.CFS_IO;
with DB.IO.Blocks.Memory_IO;

package To_Strings is

   function To_String (Column : DB.Types.Columns.String_Type) return String;

   function To_String (Row : DB.Types.Rows.String_Type) return String;

   function To_String (Time : DB.Types.Times.Number_Type) return String;

   function To_String (Key : DB.Types.Keys.Key_Type) return String;

   function To_String (Value : DB.Types.Values.Value_Type) return String;

   function To_String (Address : DB.IO.Blocks.File_IO.Address_Type)
      return String;

   function To_String (Address : DB.IO.Blocks.Direct_IO.Address_Type)
      return String;

   function To_String (Address : DB.IO.Blocks.Device_IO.Address_Type)
      return String;

   function To_String (Address : DB.IO.Blocks.CFS_IO.Address_Type)
      return String;

   function To_String (Address : DB.IO.Blocks.Memory_IO.Address_Type)
      return String;

end To_Strings;

