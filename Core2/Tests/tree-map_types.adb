package body Tree.Map_Types is

   overriding function From_Bounded
     (S : DB.Types.Values.Bounded.String_Type)
      return Value_Type
   is
      V : Value_Type;
   begin
      V.S := Values_Impl.New_String(DB.Types.Values.Bounded.To_Buffer(S));
      --V.S := S;
      return V;
   end From_Bounded;


   overriding function From_Unbounded
     (S : DB.Types.Values.Unbounded.String_Type)
      return Value_Type
   is
      V : Value_Type;
   begin
      V.S := S;
      --V.S := Values_Impl.New_String(DB.Types.Values.Bounded.To_Buffer(S));
      return V;
   end From_Unbounded;


   overriding function To_Bounded
     (V : Value_Type)
      return DB.Types.Values.Bounded.String_Type is
   begin
      return DB.Types.Values.Bounded.New_String(Values_Impl.To_Buffer(V.S));
      --return V.S;
   end To_Bounded;


   overriding function To_Unbounded
     (V : Value_Type)
      return DB.Types.Values.Unbounded.String_Type is
   begin
      --return DB.Types.Values.Unbounded.New_String(Values_Impl.To_Buffer(V.S));
      return V.S;
   end To_Unbounded;


   overriding function Equals
     (Left  : Value_Type;
      Right : DB.Tables.Value_Type'Class)
      return Boolean is
   begin
      return Right in Value_Type'Class and then
             Values_Impl."="(Left.S, Value_Type(Right).S);
   end Equals;


   overriding function Image (V : Value_Type) return String is
   begin
      declare
         Len : constant DB.Types.Values.Length_Type
             := Values_Impl.Length(Value_Type(V).S);
      begin
         return "'"& To_Strings.To_String(Value_Type(V).S) &"' "&
                "["& Len'Img &"]";
      end;
   end Image;


   function Null_Value return DB.Tables.Value_Type'Class is
   begin
      return From_Bounded(DB.Types.Values.Bounded.Empty_String);
   end Null_Value;


   function To_String (V : DB.Tables.Value_Type'Class) return String is
   begin
      return V.Image;
   end To_String;


   ----------
   -- Interface with Test_Data.

   function Get_Value
     (KV : Test_Data.Key_Value_Type)
      return DB.Tables.Value_Type'Class is
   begin
      return From_Bounded(KV.Value);
   end Get_Value;

end Tree.Map_Types;

