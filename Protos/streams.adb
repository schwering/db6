with Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

procedure Streams
is
   package Streams is
      type Stream_Type is new Ada.Streams.Root_Stream_Type with
         record
            Block  : Ada.Streams.Stream_Element_Array(1 .. 4096);
            Offset : Ada.Streams.Stream_Element_Offset := 1;
         end record;

      overriding
      procedure Write
        (Stream : in out Stream_Type;
         Item   : in     Ada.Streams.Stream_Element_Array);

      overriding
      procedure Read
        (Stream : in out Stream_Type;
         Item   :    out Ada.Streams.Stream_Element_Array;
         Last   :    out Ada.Streams.Stream_Element_Offset);

      procedure Seek
        (Stream : in out Stream_Type;
         Offset : in     Ada.Streams.Stream_Element_Offset);
   end Streams;

   package body Streams is
      overriding
      procedure Write
        (Stream : in out Stream_Type;
         Item   : in     Ada.Streams.Stream_Element_Array)
      is
         use type Ada.Streams.Stream_Element_Offset;
      begin
         Put_Line("Write"& Item'Length'Img);
         Stream.Block(Stream.Offset .. Stream.Offset + Item'Length - 1) := Item;
         Stream.Offset := Stream.Offset + Item'Length;
      end Write;

      overriding
      procedure Read
        (Stream : in out Stream_Type;
         Item   :    out Ada.Streams.Stream_Element_Array;
         Last   :    out Ada.Streams.Stream_Element_Offset)
      is
         use type Ada.Streams.Stream_Element_Offset;
      begin
         Put_Line("Read"& Item'Length'Img);
         Item := Stream.Block(Stream.Offset .. Stream.Offset + Item'Length - 1);
         Last := Item'Last;
         Stream.Offset := Stream.Offset + Item'Length;
      end Read;

      procedure Seek
        (Stream : in out Stream_Type;
         Offset : in     Ada.Streams.Stream_Element_Offset) is
      begin
         Stream.Offset := Offset;
      end Seek;
   end Streams;

   S : aliased Streams.Stream_Type;
   I : Integer := 1;
begin
   Integer'Write(S'Access, I);
   S.Seek(1);
   Integer'Read(S'Access, I);
   Put_Line("I ="& I'Img);
end Streams;

