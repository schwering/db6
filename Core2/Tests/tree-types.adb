package body Tree.Types is

   function New_Value
     (S : DB.Types.Values.Indefinite_Buffer_Type)
      return Value_Type is
   begin
      return Value_Type'(S => Values_Impl.New_String (S));
   end New_Value;


   overriding
   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : in              Value_Type)
   is
      use Values_Impl;
   begin
      Indefinite_Buffer_Type'Output (Stream, To_Buffer (Value.S));
   end Write;


   overriding
   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : out             Value_Type)
   is
      use Values_Impl;
      Buffer : constant Indefinite_Buffer_Type :=
         Indefinite_Buffer_Type'Input (Stream);
   begin
      Value.S := New_String (Buffer);
   end Read;


   function From_Bounded
     (S : DB.Types.Values.Bounded.String_Type)
      return Value_Type
   is
      V : Value_Type;
   begin
      V.S := Values_Impl.New_String (DB.Types.Values.Bounded.To_Buffer(S));
      --V.S := S;
      return V;
   end From_Bounded;


   function From_Unbounded
     (S : DB.Types.Values.Unbounded.String_Type)
      return Value_Type
   is
      V : Value_Type;
   begin
      V.S := S;
      --V.S := Values_Impl.New_String (DB.Types.Values.Bounded.To_Buffer (S));
      return V;
   end From_Unbounded;


   function To_Bounded
     (V : Value_Type)
      return DB.Types.Values.Bounded.String_Type is
   begin
      return DB.Types.Values.Bounded.New_String (Values_Impl.To_Buffer (V.S));
      --return V.S;
   end To_Bounded;


   function To_Unbounded
     (V : Value_Type)
      return DB.Types.Values.Unbounded.String_Type is
   begin
      --return DB.Types.Values.Unbounded.New_String (Values_Impl.To_Buffer (V.S));
      return V.S;
   end To_Unbounded;


   overriding
   function Equals
     (Left, Right : Value_Type)
      return Boolean is
   begin
      return Values_Impl."=" (Left.S, Right.S);
   end Equals;


   overriding
   function Image (V : Value_Type) return String is
   begin
      declare
         Len : constant DB.Types.Values.Length_Type
             := Values_Impl.Length (V.S);
      begin
         return "'"& To_Strings.To_String (V.S) &"' "&
                "["& Len'Img &"]";
      end;
   end Image;


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

end Tree.Types;

