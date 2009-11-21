-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Unchecked_Conversion;

package body To_Strings is

   function To_String (S : DB.Types.Strings.Bounded.String_Type)
      return String is
   begin
      return String(DB.Types.Strings.Bounded.To_Buffer(S));
   end To_String;


   function To_String (S : DB.Types.Strings.Unbounded.String_Type)
      return String is
   begin
      return String(DB.Types.Strings.Unbounded.To_Buffer(S));
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


   function To_String (Value : DB.Types.Values.Bounded.String_Type)
      return String
   is
      use DB.Types.Values;
      use DB.Types.Values.Bounded;
      Buf : constant Indefinite_Buffer_Type := To_Buffer(Value);
      type Buffer_Type is new Indefinite_Buffer_Type(Buf'Range);
      type Fixed_String_Type is new String(Buf'Range);
      function Convert is new Ada.Unchecked_Conversion
        (Buffer_Type, Fixed_String_Type);
   begin
      return String(Convert(Buffer_Type(Buf)));
   end To_String;


   function To_String (Value : DB.Types.Values.Unbounded.String_Type)
      return String
   is
      use DB.Types.Values;
      use DB.Types.Values.Unbounded;
      Buf : constant Indefinite_Buffer_Type := To_Buffer(Value);
      type Buffer_Type is new Indefinite_Buffer_Type(Buf'Range);
      type Fixed_String_Type is new String(Buf'Range);
      function Convert is new Ada.Unchecked_Conversion
        (Buffer_Type, Fixed_String_Type);
   begin
      return String(Convert(Buffer_Type(Buf)));
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


