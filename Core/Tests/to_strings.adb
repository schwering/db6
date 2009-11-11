-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

package body To_Strings is

   function To_String (Column : DB.Types.Columns.String_Type) return String is
   begin
      return String(DB.Types.Columns.To_String(Column));
   end To_String;

   function To_String (Row : DB.Types.Rows.String_Type) return String is
   begin
      return String(DB.Types.Rows.To_String(Row));
   end To_String;

   function To_String (Time : DB.Types.Times.Number_Type) return String is
   begin
      return DB.Types.Times.Number_Type'Image(Time);
   end To_String;

   function To_String (Key : DB.Types.Keys.Key_Type) return String is
   begin
      return To_String(Key.Row) &" | "&
             To_String(Key.Column) &" |"&
             To_String(Key.Time);
   end To_String;

   function To_String (Value : DB.Types.Values.Value_Type) return String is
   begin
      return DB.Types.Values.Value_Type'Image(Value);
   end To_String;

   function To_String (Address : DB.IO.Blocks.File_IO.Address_Type)
      return String is
   begin
      return DB.IO.Blocks.File_IO.Address_Type'Image(Address);
   end To_String;

   function To_String (Address : DB.IO.Blocks.Direct_IO.Address_Type)
      return String is
   begin
      return DB.IO.Blocks.Direct_IO.Address_Type'Image(Address);
   end To_String;

   function To_String (Address : DB.IO.Blocks.Device_IO.Address_Type)
      return String is
   begin
      return DB.IO.Blocks.Device_IO.Address_Type'Image(Address);
   end To_String;

   function To_String (Address : DB.IO.Blocks.CFS_IO.Address_Type)
     return String is
   begin
      return DB.IO.Blocks.CFS_IO.Address_Type'Image(Address);
   end To_String;

   function To_String (Address : DB.IO.Blocks.Memory_IO.Address_Type)
      return String is
   begin
      return DB.IO.Blocks.Memory_IO.Address_Type'Image(Address);
   end To_String;

end To_Strings;


