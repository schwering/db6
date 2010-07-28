with DB.Maps.Tag_Map;
with DB.Types.Values.Bounded.Streams;

package body Tree.Types is

   overriding
   function New_Value
     (Params : not null access DB.Maps.Value_Parameters_Type)
      return Value_Type is
   begin
      return (DB.Maps.Value_Utils.String_Values.New_Value (Params) with
              null record);
   end New_Value;


   function New_Value
     (S : String)
      return Value_Type is
   begin
      return (DB.Maps.Value_Utils.String_Values.New_Value (S) with
              null record);
   end New_Value;


   function New_Value
     (S : DB.Types.Values.Indefinite_Buffer_Type)
      return Value_Type is
   begin
      return New_Value (String (S));
   end New_Value;


   function From_Bounded
     (S : DB.Types.Values.Bounded.String_Type)
      return Value_Type is
   begin
      return New_Value (DB.Types.Values.Bounded.To_Buffer (S));
   end From_Bounded;


   function From_Unbounded
     (S : DB.Types.Values.Unbounded.String_Type)
      return Value_Type is
   begin
      return New_Value (DB.Types.Values.Unbounded.To_Buffer (S));
   end From_Unbounded;


   function To_Bounded
     (V : Value_Type)
      return DB.Types.Values.Bounded.String_Type is
   begin
      return DB.Types.Values.Bounded.New_String
              (DB.Types.Values.Bounded.Indefinite_Buffer_Type (V.Image));
   end To_Bounded;


   function To_Unbounded
     (V : Value_Type)
      return DB.Types.Values.Unbounded.String_Type is
   begin
      return DB.Types.Values.Unbounded.New_String
              (DB.Types.Values.Unbounded.Indefinite_Buffer_Type (V.Image));
   end To_Unbounded;


   function Null_Value return Value_Type is
   begin
      return From_Bounded (DB.Types.Values.Bounded.Empty_String);
   end Null_Value;


   function To_String (V : DB.Maps.Value_Type'Class) return String is
   begin
      return V.Image;
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

begin
   DB.Maps.Tag_Map.Register (Value_Type'Tag);
   DB.Maps.Tag_Map.Seal;
end Tree.Types;

