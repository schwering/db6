-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body DB.Maps.Values.Gen_Arrays is

   type Length_Type is mod 2**16;
   for Length_Type'Size use 16;

   procedure Free is new Ada.Unchecked_Deallocation
     (Array_Type, Array_Ref_Type);


   procedure Initialize (Value : in out Value_Type) is
   begin
      Value.Str := null;
   end Initialize;


   procedure Adjust (Value : in out Value_Type) is
   begin
      if Value.Str /= null then
         declare
            S : constant Array_Ref_Type := new Array_Type'(Value.Str.all);
         begin
            Value.Str := S;
         end;
      end if;
   end Adjust;


   procedure Finalize (Value : in out Value_Type) is
   begin
      if Value.Str /= null then
         Free (Value.Str);
      end if;
   end Finalize;


   function New_Value
     (Params : not null access DB.Maps.Value_Parameters_Type)
      return Value_Type
   is
      pragma Unreferenced (Params);
   begin
      return (Maps.Value_Type with
              Str => null);
   end New_Value;


   function New_Value (S : Array_Type) return Value_Type is
   begin
      return (Maps.Value_Type with
              Str => new Array_Type'(S));
   end New_Value;


   function Size_Bound
      (Value : Value_Type)
       return Ada.Streams.Stream_Element_Offset
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      return 2 + Value.Str'Length;
   end Size_Bound;


   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : in              Value_Type)
   is
      Length : constant Length_Type := Length_Type'(Value.Str'Length);
   begin
      Length_Type'Write (Stream, Length);
      Array_Type'Write (Stream, Value.Str.all);
   end Write;


   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : out             Value_Type)
   is
      Length : Length_Type;
   begin
      if Value.Str /= null then
         Free (Value.Str);
      end if;
      Length_Type'Read (Stream, Length);
      Value.Str := new Array_Type (1 .. Natural (Length));
      Array_Type'Read (Stream, Value.Str.all);
   end Read;


   function Equals (A, B : Value_Type) return Boolean is
   begin
      return A.Str.all = B.Str.all;
   end Equals;


   function Image (Value : Value_Type) return String is
      use Ada.Strings.Unbounded;
      S : Unbounded_String := To_Unbounded_String ("");
   begin
      for I in Value.Str'Range loop
         Append (S, Item_Type'Image (Value.Str (I)));
      end loop;
      return To_String (S);
   end Image;

end DB.Maps.Values.Gen_Arrays;

