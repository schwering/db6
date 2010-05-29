-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Conversion;

package body DB.Types.Gen_Strings.Gen_Bounded.Gen_Streams is

   function New_Stream
     (String_Ref : String_Ref_Type)
      return Stream_Type is
   begin
      return Stream_Type'(Ada.Streams.Root_Stream_Type with
                          String => String_Ref,
                          Pos    => String_Ref.Buffer'First);
   end New_Stream;


   overriding
   procedure Write
     (Stream : in out Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array)
   is
      Length : constant Natural := Item'Size / Item_Type'Size;

      subtype Constr_Stream_Elem_Array is
         Ada.Streams.Stream_Element_Array(Item'Range);
      subtype Constr_Buffer_Type is
         Indefinite_Buffer_Type(1 .. Length);
      function Convert is new Ada.Unchecked_Conversion
        (Constr_Stream_Elem_Array, Constr_Buffer_Type);
   begin
      Stream.String.Buffer(Stream.Pos .. Stream.Pos + Length - 1) :=
         Convert(Item);
      Stream.String.Length := Stream.Pos + Length - 1;
      Stream.Pos := Stream.Pos + Length;
   end Write;


   overriding
   procedure Read
     (Stream : in out Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset)
   is
      Length : constant Natural := Item'Size / Item_Type'Size;

      subtype Constr_Stream_Elem_Array is
         Ada.Streams.Stream_Element_Array(Item'Range);
      subtype Constr_Buffer_Type is
         Indefinite_Buffer_Type(1 .. Length);
      function Convert is new Ada.Unchecked_Conversion
        (Constr_Buffer_Type, Constr_Stream_Elem_Array);
   begin
      Item :=
         Convert(Stream.String.Buffer(Stream.Pos .. Stream.Pos + Length - 1));
      Last := Item'Last;
      Stream.Pos := Stream.Pos + Length;
   end Read;


   procedure Seek
     (Stream : in out Stream_Type;
      Pos    : in     Ada.Streams.Stream_Element_Offset) is
   begin
      Stream.Pos := Length_Type(Pos);
   end Seek;


   function Remaining
      (Stream : Stream_Type)
       return Length_Type is
   begin
      return Length(Stream.String.all) - Stream.Pos + 1;
   end Remaining;

end DB.Types.Gen_Strings.Gen_Bounded.Gen_Streams;

