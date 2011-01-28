package body Tree.Types is

   function New_Value (S : String) return Value_Type is
   begin
      return DB.Types.Values.New_Value (S);
   end New_Value;


   function New_Value
     (S : DB.Types.Byte_Arrays.Indefinite_Buffer_Type)
      return Value_Type is
   begin
      return New_Value (String (S));
   end New_Value;


   function From_Bounded
     (S : DB.Types.Byte_Arrays.Bounded.String_Type)
      return Value_Type is
   begin
      return New_Value (DB.Types.Byte_Arrays.Bounded.To_Buffer (S));
   end From_Bounded;


   function From_Unbounded
     (S : DB.Types.Byte_Arrays.Unbounded.String_Type)
      return Value_Type is
   begin
      return New_Value (DB.Types.Byte_Arrays.Unbounded.To_Buffer (S));
   end From_Unbounded;


   function To_Bounded
     (V : Value_Type)
      return DB.Types.Byte_Arrays.Bounded.String_Type is
   begin
      return DB.Types.Byte_Arrays.Bounded.New_String
              (DB.Types.Byte_Arrays.Bounded.Indefinite_Buffer_Type
                  (DB.Types.Values.Image (V)));
   end To_Bounded;


   function To_Unbounded
     (V : Value_Type)
      return DB.Types.Byte_Arrays.Unbounded.String_Type is
   begin
      return DB.Types.Byte_Arrays.Unbounded.New_String
              (DB.Types.Byte_Arrays.Unbounded.Indefinite_Buffer_Type
               (DB.Types.Values.Image (V)));
   end To_Unbounded;


   function Null_Value return Value_Type is
   begin
      return From_Bounded (DB.Types.Byte_Arrays.Bounded.Empty_String);
   end Null_Value;


   function To_String (V : DB.Types.Values.Value_Type) return String is
   begin
      return DB.Types.Values.Image (V);
   end To_String;


   function Key (KV : Key_Value_Type) return DB.Types.Keys.Key_Type is
   begin
      return KV.Key;
   end;


   function Value (KV : Key_Value_Type)
      return Value_Type is
   begin
      return KV.Value;
   end;

end Tree.Types;

