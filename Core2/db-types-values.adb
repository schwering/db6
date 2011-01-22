-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body DB.Types.Values is

   function Nothing_Value return Value_Type is
   begin
      return Value_Type'(Tag => Nothing_Value, others => <>);
   end Nothing_Value;


   function New_Value (Bool : Booleans.Discrete_Type) return Value_Type is
   begin
      return Value_Type'(Tag => Boolean_Value, Bool => Bool);
   end New_Value;


   function New_Value (Int : Integers.Discrete_Type) return Value_Type is
   begin
      return Value_Type'(Tag => Integer_Value, Int => Int);
   end New_Value;


   function New_Value (Long_Int : Long_Integers.Discrete_Type)
      return Value_Type is
   begin
      return Value_Type'(Tag => Long_Integer_Value, Long_Int => Long_Int);
   end New_Value;


   function New_Value (Real : Reals.Real_Type) return Value_Type is
   begin
      return Value_Type'(Tag => Real_Value, Real => Real);
   end New_Value;


   function New_Value (Long_Real : Long_Reals.Real_Type) return Value_Type is
   begin
      return Value_Type'(Tag => Long_Real_Value, Long_Real => Long_Real);
   end New_Value;


   function New_Value (S : String) return Value_Type is
      use Strings;
   begin
      if S'Length in Bounded.Length_Type'Range then
         return (Bounded_String_Value,
                 Bounded.New_String (Indefinite_Buffer_Type (S)));
      else
         return (Unbounded_String_Value,
                 Unbounded.New_String (Indefinite_Buffer_Type (S)));
      end if;
   end New_Value;


   function New_Value (Bounded_String : Strings.Bounded.String_Type)
      return Value_Type is
   begin
      return Value_Type'(Tag => Bounded_String_Value,
                         Bounded_String => Bounded_String);
   end New_Value;


   function New_Value (Unbounded_String : Strings.Unbounded.String_Type)
      return Value_Type is
   begin
      return Value_Type'(Tag => Unbounded_String_Value,
                         Unbounded_String => Unbounded_String);
   end New_Value;


   function New_Value (Bounded_Byte_Array : Byte_Arrays.Bounded.String_Type)
      return Value_Type is
   begin
      return Value_Type'(Tag => Bounded_Byte_Array_Value,
                         Bounded_Byte_Array => Bounded_Byte_Array);
   end New_Value;


   function New_Value (Unbounded_Byte_Array : Byte_Arrays.Unbounded.String_Type)
      return Value_Type is
   begin
      return Value_Type'(Tag => Unbounded_Byte_Array_Value,
                         Unbounded_Byte_Array => Unbounded_Byte_Array);
   end New_Value;


   function New_Value (Key : Keys.Key_Type) return Value_Type is
   begin
      return Value_Type'(Tag => Key_Value, Key => Key);
   end New_Value;


   function "=" (Left, Right : Value_Type) return Boolean
   is
      use type Nothings.Nothing_Type;
      use type Booleans.Discrete_Type;
      use type Integers.Discrete_Type;
      use type Long_Integers.Discrete_Type;
      use type Reals.Real_Type;
      use type Long_Reals.Real_Type;
      use type Strings.Bounded.String_Type;
      use type Strings.Unbounded.String_Type;
      use type Byte_Arrays.Bounded.String_Type;
      use type Byte_Arrays.Unbounded.String_Type;
      use type Keys.Key_Type;
   begin
      if Left.Tag /= Right.Tag then
         return False;
      end if;
      case Left.Tag is
         when Nothing_Value =>
            return Left.Nothing = Right.Nothing;
         when Boolean_Value =>
            return Left.Bool = Right.Bool;
         when Integer_Value =>
            return Left.Int = Right.Int;
         when Long_Integer_Value =>
            return Left.Long_Int = Right.Long_Int;
         when Real_Value =>
            return Left.Real = Right.Real;
         when Long_Real_Value =>
            return Left.Long_Real = Right.Long_Real;
         when Bounded_String_Value =>
            return Left.Bounded_String = Right.Bounded_String;
         when Unbounded_String_Value =>
            return Left.Unbounded_String = Right.Unbounded_String;
         when Bounded_Byte_Array_Value =>
            return Left.Bounded_Byte_Array = Right.Bounded_Byte_Array;
         when Unbounded_Byte_Array_Value =>
            return Left.Unbounded_Byte_Array = Right.Unbounded_Byte_Array;
         when Key_Value =>
            return Left.Key = Right.Key;
      end case;
   end "=";


   function New_Read_Context return Read_Context_Type is
   begin
      return (Tags.New_Read_Context,
              Nothings.New_Read_Context,
              Booleans.New_Read_Context,
              Integers.New_Read_Context,
              Long_Integers.New_Read_Context,
              Reals.New_Read_Context,
              Long_Reals.New_Read_Context,
              Bounded_Strings_Serialization.New_Read_Context,
              Unbounded_Strings_Serialization.New_Read_Context,
              Bounded_Byte_Arrays_Serialization.New_Read_Context,
              Unbounded_Byte_Arrays_Serialization.New_Read_Context,
              Keys.New_Read_Context);
   end New_Read_Context;


   function New_Write_Context return Write_Context_Type is
   begin
      return (Tags.New_Write_Context,
              Nothings.New_Write_Context,
              Booleans.New_Write_Context,
              Integers.New_Write_Context,
              Long_Integers.New_Write_Context,
              Reals.New_Write_Context,
              Long_Reals.New_Write_Context,
              Bounded_Strings_Serialization.New_Write_Context,
              Unbounded_Strings_Serialization.New_Write_Context,
              Bounded_Byte_Arrays_Serialization.New_Write_Context,
              Unbounded_Byte_Arrays_Serialization.New_Write_Context,
              Keys.New_Write_Context);
   end New_Write_Context;


   function Size_Bound (Value : Value_Type) return Blocks.Size_Type
   is
      use type Blocks.Size_Type;
      N : Blocks.Size_Type;
   begin
      N := Tags.Size_Bound (Value.Tag);
      case Value.Tag is
         when Nothing_Value =>
            return N + Nothings.Size_Bound (Value.Nothing);
         when Boolean_Value =>
            return N + Booleans.Size_Bound (Value.Bool);
         when Integer_Value =>
            return N + Integers.Size_Bound (Value.Int);
         when Long_Integer_Value =>
            return N + Long_Integers.Size_Bound (Value.Long_Int);
         when Real_Value =>
            return N + Reals.Size_Bound (Value.Real);
         when Long_Real_Value =>
            return N + Long_Reals.Size_Bound (Value.Long_Real);
         when Bounded_String_Value =>
            return N + Bounded_Strings_Serialization.Size_Bound
              (Value.Bounded_String);
         when Unbounded_String_Value =>
            return N + Unbounded_Strings_Serialization.Size_Bound
              (Value.Unbounded_String);
         when Bounded_Byte_Array_Value =>
            return N + Bounded_Byte_Arrays_Serialization.Size_Bound
              (Value.Bounded_Byte_Array);
         when Unbounded_Byte_Array_Value =>
            return N + Unbounded_Byte_Arrays_Serialization.Size_Bound
              (Value.Unbounded_Byte_Array);
         when Key_Value =>
            return N + Keys.Size_Bound (Value.Key);
      end case;
   end Size_Bound;


   procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Value   : in     Value_Type) is
   begin
      Tags.Write (Context.Tag, Block, Cursor, Value.Tag);
      case Value.Tag is
         when Nothing_Value =>
            Nothings.Write
              (Context.Nothing, Block, Cursor, Value.Nothing);
         when Boolean_Value =>
            Booleans.Write
              (Context.Bool, Block, Cursor, Value.Bool);
         when Integer_Value =>
            Integers.Write
              (Context.Int, Block, Cursor, Value.Int);
         when Long_Integer_Value =>
            Long_Integers.Write
              (Context.Long_Int, Block, Cursor, Value.Long_Int);
         when Real_Value =>
            Reals.Write
              (Context.Real, Block, Cursor, Value.Real);
         when Long_Real_Value =>
            Long_Reals.Write
              (Context.Long_Real, Block, Cursor, Value.Long_Real);
         when Bounded_String_Value =>
            Bounded_Strings_Serialization.Write
              (Context.Bounded_String, Block, Cursor, Value.Bounded_String);
         when Unbounded_String_Value =>
            Unbounded_Strings_Serialization.Write
              (Context.Unbounded_String, Block, Cursor, Value.Unbounded_String);
         when Bounded_Byte_Array_Value =>
            Bounded_Byte_Arrays_Serialization.Write
              (Context.Bounded_Byte_Array, Block, Cursor,
               Value.Bounded_Byte_Array);
         when Unbounded_Byte_Array_Value =>
            Unbounded_Byte_Arrays_Serialization.Write
              (Context.Unbounded_Byte_Array, Block, Cursor,
               Value.Unbounded_Byte_Array);
         when Key_Value =>
            Keys.Write
              (Context.Key, Block, Cursor, Value.Key);
      end case;
   end Write;


   procedure Read
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Value   :    out Value_Type)
   is
      Tag : Tag_Type;
   begin
      Tags.Read (Context.Tag, Block, Cursor, Tag);
      case Tag is
         when Nothing_Value =>
            Value := Value_Type'(Tag => Nothing_Value, others => <>);
            Nothings.Read
              (Context.Nothing, Block, Cursor, Value.Nothing);
         when Boolean_Value =>
            Value := Value_Type'(Tag => Boolean_Value, others => <>);
            Booleans.Read
              (Context.Bool, Block, Cursor, Value.Bool);
         when Integer_Value =>
            Value := Value_Type'(Tag => Integer_Value, others => <>);
            Integers.Read
              (Context.Int, Block, Cursor, Value.Int);
         when Long_Integer_Value =>
            Value := Value_Type'(Tag => Long_Integer_Value, others => <>);
            Long_Integers.Read
              (Context.Long_Int, Block, Cursor, Value.Long_Int);
         when Real_Value =>
            Value := Value_Type'(Tag => Real_Value, others => <>);
            Reals.Read
              (Context.Real, Block, Cursor, Value.Real);
         when Long_Real_Value =>
            Value := Value_Type'(Tag => Long_Real_Value, others => <>);
            Long_Reals.Read
              (Context.Long_Real, Block, Cursor, Value.Long_Real);
         when Bounded_String_Value =>
            Value := Value_Type'(Tag => Bounded_String_Value, others => <>);
            Bounded_Strings_Serialization.Read
              (Context.Bounded_String, Block, Cursor, Value.Bounded_String);
         when Unbounded_String_Value =>
            Value := Value_Type'(Tag => Unbounded_String_Value, others => <>);
            Unbounded_Strings_Serialization.Read
              (Context.Unbounded_String, Block, Cursor, Value.Unbounded_String);
         when Bounded_Byte_Array_Value =>
            Value := Value_Type'(Tag => Bounded_Byte_Array_Value, others => <>);
            Bounded_Byte_Arrays_Serialization.Read
              (Context.Bounded_Byte_Array, Block, Cursor,
               Value.Bounded_Byte_Array);
         when Unbounded_Byte_Array_Value =>
            Value := Value_Type'(Tag => Unbounded_Byte_Array_Value,
                                 others => <>);
            Unbounded_Byte_Arrays_Serialization.Read
              (Context.Unbounded_Byte_Array, Block, Cursor,
               Value.Unbounded_Byte_Array);
         when Key_Value =>
            Value := Value_Type'(Tag => Key_Value,
                                 others => <>);
            Keys.Read
              (Context.Key, Block, Cursor, Value.Key);
      end case;
   end Read;


   procedure Skip
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type)
   is
      Tag : Tag_Type;
   begin
      Tags.Read (Context.Tag, Block, Cursor, Tag);
      case Tag is
         when Nothing_Value =>
            Nothings.Skip
              (Context.Nothing, Block, Cursor);
         when Boolean_Value =>
            Booleans.Skip
              (Context.Bool, Block, Cursor);
         when Integer_Value =>
            Integers.Skip
              (Context.Int, Block, Cursor);
         when Long_Integer_Value =>
            Long_Integers.Skip
              (Context.Long_Int, Block, Cursor);
         when Real_Value =>
            Reals.Skip
              (Context.Real, Block, Cursor);
         when Long_Real_Value =>
            Long_Reals.Skip
              (Context.Long_Real, Block, Cursor);
         when Bounded_String_Value =>
            Bounded_Strings_Serialization.Skip
              (Context.Bounded_String, Block, Cursor);
         when Unbounded_String_Value =>
            Unbounded_Strings_Serialization.Skip
              (Context.Unbounded_String, Block, Cursor);
         when Bounded_Byte_Array_Value =>
            Bounded_Byte_Arrays_Serialization.Skip
              (Context.Bounded_Byte_Array, Block, Cursor);
         when Unbounded_Byte_Array_Value =>
            Unbounded_Byte_Arrays_Serialization.Skip
              (Context.Unbounded_Byte_Array, Block, Cursor);
         when Key_Value =>
            Keys.Skip
              (Context.Key, Block, Cursor);
      end case;
   end Skip;


   function Image (Value : Value_Type) return String is
   begin
      case Value.Tag is
         when Nothing_Value =>
            return Nothings.Image (Value.Nothing);
         when Boolean_Value =>
            return Booleans.Image (Value.Bool);
         when Integer_Value =>
            return Integers.Image (Value.Int);
         when Long_Integer_Value =>
            return Long_Integers.Image (Value.Long_Int);
         when Real_Value =>
            return Reals.Image (Value.Real);
         when Long_Real_Value =>
            return Long_Reals.Image (Value.Long_Real);
         when Bounded_String_Value =>
            return Strings.Bounded.Image (Value.Bounded_String);
         when Unbounded_String_Value =>
            return Strings.Unbounded.Image (Value.Unbounded_String);
         when Bounded_Byte_Array_Value =>
            return Byte_Arrays.Bounded.Image (Value.Bounded_Byte_Array);
         when Unbounded_Byte_Array_Value =>
            return Byte_Arrays.Unbounded.Image (Value.Unbounded_Byte_Array);
         when Key_Value =>
            return Keys.Image (Value.Key);
      end case;
   end Image;

end DB.Types.Values;

