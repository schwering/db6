with Ada.Unchecked_Conversion;

package body Tree.To_Strings is

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


   function To_String (Time : DB.Types.Times.Time_Type) return String is
   begin
      return DB.Types.Times.Time_Type'Image(Time);
   end To_String;


   function To_String (Key : DB.Types.Keys.Key_Type) return String is
   begin
      return To_String(Key.Row) &" | "&
             To_String(Key.Column) &" |"&
             To_String(Key.Time);
   end To_String;


   function To_String (Value : DB.Types.Byte_Arrays.Bounded.String_Type)
      return String
   is
      use DB.Types.Byte_Arrays;
      use DB.Types.Byte_Arrays.Bounded;
      Buf : constant DB.Types.Byte_Arrays.Bounded.Indefinite_Buffer_Type :=
         To_Buffer(Value);
      type Buffer_Type is
         new DB.Types.Byte_Arrays.Bounded.Indefinite_Buffer_Type(Buf'Range);
      type Fixed_String_Type is new String(Buf'Range);
      function Convert is new Ada.Unchecked_Conversion
        (Buffer_Type, Fixed_String_Type);
   begin
      return String(Convert(Buffer_Type(Buf)));
   end To_String;


   function To_String (Value : DB.Types.Byte_Arrays.Unbounded.String_Type)
      return String
   is
      use DB.Types.Byte_Arrays;
      use DB.Types.Byte_Arrays.Unbounded;
      Buf : constant DB.Types.Byte_Arrays.Bounded.Indefinite_Buffer_Type :=
         To_Buffer(Value);
      type Buffer_Type is
         new DB.Types.Byte_Arrays.Bounded.Indefinite_Buffer_Type(Buf'Range);
      type Fixed_String_Type is new String(Buf'Range);
      function Convert is new Ada.Unchecked_Conversion
        (Buffer_Type, Fixed_String_Type);
   begin
      return String(Convert(Buffer_Type(Buf)));
   end To_String;


   function To_String (Address : DB.Blocks.Local_IO.Address_Type)
      return String is
   begin
      return DB.Blocks.Local_IO.Address_Type'Image(Address);
   end To_String;


   function To_String (Address : DB.Blocks.Memory_IO.Address_Type)
      return String is
   begin
      return DB.Blocks.Memory_IO.Address_Type'Image(Address);
   end To_String;

end Tree.To_Strings;

