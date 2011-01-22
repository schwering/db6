-- Abstract:
--
-- Value type. A Value consists of a row, a column and a timestamp.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Blocks;
with DB.Blocks.Gen_Values_Signature;
with DB.Types.Byte_Arrays.Bounded;
with DB.Types.Byte_Arrays.Unbounded;
with DB.Types.Gen_Discretes;
with DB.Types.Gen_Reals;
with DB.Types.Keys;
with DB.Types.Nothings;
with DB.Types.Strings.Bounded;
with DB.Types.Strings.Unbounded;

package DB.Types.Values is
   pragma Preelaborate;

   package Bounded_Strings_Serialization
   renames Strings.Bounded.Uncompressed;

   package Unbounded_Strings_Serialization
   renames Strings.Unbounded.Uncompressed;

   package Bounded_Byte_Arrays_Serialization
   renames Byte_Arrays.Bounded.Uncompressed;

   package Unbounded_Byte_Arrays_Serialization
   renames Byte_Arrays.Unbounded.Uncompressed;

   type Tag_Type is
     (Nothing_Value,
      Boolean_Value,
      Integer_Value,
      Long_Integer_Value,
      Real_Value,
      Long_Real_Value,
      Bounded_String_Value,
      Unbounded_String_Value,
      Bounded_Byte_Array_Value,
      Unbounded_Byte_Array_Value,
      Key_Value);
   for Tag_Type'Size use 16;

   package Tags is new Gen_Discretes (Tag_Type);

   package Booleans is new Gen_Discretes (Boolean);
   package Integers is new Gen_Discretes (Integer);
   package Long_Integers is new Gen_Discretes (Long_Integer);
   package Reals is new Gen_Reals (Float);
   package Long_Reals is new Gen_Reals (Long_Float);

   type Value_Type (Tag : Tag_Type := Nothing_Value) is
      record
         case Tag is
            when Nothing_Value =>
               Nothing : Nothings.Nothing_Type;
            when Boolean_Value =>
               Bool : Booleans.Discrete_Type;
            when Integer_Value =>
               Int : Integers.Discrete_Type;
            when Long_Integer_Value =>
               Long_Int : Long_Integers.Discrete_Type;
            when Real_Value =>
               Real : Reals.Real_Type;
            when Long_Real_Value =>
               Long_Real : Long_Reals.Real_Type;
            when Bounded_String_Value =>
               Bounded_String : Strings.Bounded.String_Type;
            when Unbounded_String_Value =>
               Unbounded_String : Strings.Unbounded.String_Type;
            when Bounded_Byte_Array_Value =>
               Bounded_Byte_Array : Byte_Arrays.Bounded.String_Type;
            when Unbounded_Byte_Array_Value =>
               Unbounded_Byte_Array : Byte_Arrays.Unbounded.String_Type;
            when Key_Value =>
               Key : Keys.Key_Type;
         end case;
      end record;

   type Read_Context_Type is
      record
         Tag : Tags.Read_Context_Type;
         Nothing : Nothings.Read_Context_Type;
         Bool : Booleans.Read_Context_Type;
         Int : Integers.Read_Context_Type;
         Long_Int : Long_Integers.Read_Context_Type;
         Real : Reals.Read_Context_Type;
         Long_Real : Long_Reals.Read_Context_Type;
         Bounded_String : Bounded_Strings_Serialization.Read_Context_Type;
         Unbounded_String : Unbounded_Strings_Serialization.Read_Context_Type;
         Bounded_Byte_Array :
            Bounded_Byte_Arrays_Serialization.Read_Context_Type;
         Unbounded_Byte_Array :
            Unbounded_Byte_Arrays_Serialization.Read_Context_Type;
         Key : Keys.Read_Context_Type;
      end record;

   type Write_Context_Type is
      record
         Tag : Tags.Write_Context_Type;
         Nothing : Nothings.Write_Context_Type;
         Bool : Booleans.Write_Context_Type;
         Int : Integers.Write_Context_Type;
         Long_Int : Long_Integers.Write_Context_Type;
         Real : Reals.Write_Context_Type;
         Long_Real : Long_Reals.Write_Context_Type;
         Bounded_String : Bounded_Strings_Serialization.Write_Context_Type;
         Unbounded_String : Unbounded_Strings_Serialization.Write_Context_Type;
         Bounded_Byte_Array :
            Bounded_Byte_Arrays_Serialization.Write_Context_Type;
         Unbounded_Byte_Array :
            Unbounded_Byte_Arrays_Serialization.Write_Context_Type;
         Key : Keys.Write_Context_Type;
      end record;

   Is_Context_Free_Serialization : constant Boolean :=
      Nothings.Is_Context_Free_Serialization and
      Booleans.Is_Context_Free_Serialization and
      Long_Integers.Is_Context_Free_Serialization and
      Long_Integers.Is_Context_Free_Serialization and
      Long_Reals.Is_Context_Free_Serialization and
      Long_Reals.Is_Context_Free_Serialization and
      Bounded_Strings_Serialization.Is_Context_Free_Serialization and
      Unbounded_Strings_Serialization.Is_Context_Free_Serialization and
      Unbounded_Byte_Arrays_Serialization.Is_Context_Free_Serialization and
      Unbounded_Byte_Arrays_Serialization.Is_Context_Free_Serialization and
      Keys.Is_Context_Free_Serialization;


   function Nothing_Value return Value_Type;
   function New_Value (Bool : Booleans.Discrete_Type) return Value_Type;
   function New_Value (Int : Integers.Discrete_Type) return Value_Type;
   function New_Value (Long_Int : Long_Integers.Discrete_Type)
      return Value_Type;
   function New_Value (Real : Reals.Real_Type) return Value_Type;
   function New_Value (Long_Real : Long_Reals.Real_Type) return Value_Type;
   function New_Value (Bounded_String : Strings.Bounded.String_Type)
      return Value_Type;
   function New_Value (Unbounded_String : Strings.Unbounded.String_Type)
      return Value_Type;
   function New_Value (Bounded_Byte_Array : Byte_Arrays.Bounded.String_Type)
      return Value_Type;
   function New_Value (Unbounded_Byte_Array : Byte_Arrays.Unbounded.String_Type)
      return Value_Type;
   function New_Value (Key : Keys.Key_Type) return Value_Type;

   function "="  (Left, Right : Value_Type) return Boolean;

   function New_Read_Context return Read_Context_Type;

   function New_Write_Context return Write_Context_Type;

   function Size_Bound (Value : Value_Type) return Blocks.Size_Type;

   procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Value     : in     Value_Type);

   procedure Read
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Value     :    out Value_Type);

   procedure Skip
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type);

   function Image (Value : Value_Type) return String;

   package Values_Signature is new Blocks.Gen_Values_Signature
     (Value_Type         => Value_Type,
      Read_Context_Type  => Read_Context_Type,
      Write_Context_Type => Write_Context_Type);

private
   pragma Inline (Nothing_Value);
   pragma Inline (New_Value);
   pragma Inline (New_Read_Context);
   pragma Inline (New_Write_Context);

end DB.Types.Values;

