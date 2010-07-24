-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Maps.Value_Utils.String_Values is

   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Ref_Type);


   procedure Initialize (Value : in out Value_Type) is
   begin
      Value.Str := null;
   end Initialize;


   procedure Adjust (Value : in out Value_Type) is
   begin
      if Value.Str /= null then
         declare
            S : constant String_Ref_Type := new String'(Value.Str.all);
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


   function Make (S : String) return Value_Type is
   begin
      return (AF.Controlled with new String'(S));
   end Make;


   procedure Write
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : in              Value_Type) is
   begin
      String'Output (Stream, Value.Str.all);
   end Write;


   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Value  : out             Value_Type) is
   begin
      if Value.Str /= null then
         Free (Value.Str);
      end if;
      Value.Str := new String'(String'Input (Stream));
   end Read;


   function Equals (A, B : Value_Type) return Boolean is
   begin
      return A.Str.all = B.Str.all;
   end Equals;


   function Image (Value : Value_Type) return String is
   begin
      return Value.Str.all;
   end Image;

end DB.Maps.Value_Utils.String_Values;

