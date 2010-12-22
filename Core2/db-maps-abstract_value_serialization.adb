-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Tags;
with Ada.Tags.Generic_Dispatching_Constructor;

with DB.Blocks.Streams;
with DB.Maps.Tag_Map;

package body DB.Maps.Abstract_Value_Serialization is

   function New_Read_Context return Read_Context_Type is
   begin
      return (null record);
   end New_Read_Context;


   function New_Write_Context return Write_Context_Type is
   begin
      return (null record);
   end New_Write_Context;


   function Size_Bound (Value : Value_Wrapper_Type) return Blocks.Size_Type
   is
      use type Blocks.Size_Type;
   begin
      return Blocks.Bits_To_Units (Tag_Map.Tid_Type'Size) +
             Blocks.Size_Type (Value.Ref.Size_Bound);
   end Size_Bound;


   procedure Read
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Value   :    out Value_Wrapper_Type)
   is
      pragma Unreferenced (Context);

      Tid    : Tag_Map.Tid_Type;
      Stream : aliased Blocks.Streams.Stream_Type :=
         Blocks.Streams.New_Stream (Block'Unrestricted_Access,
                                    Cursor'Unrestricted_Access);
   begin
      Tag_Map.Tid_Type'Read (Stream'Access, Tid);
      declare
         function Constructor is new Ada.Tags.Generic_Dispatching_Constructor
           (Value_Type, Value_Parameters_Type, New_Value);

         Tag : constant Ada.Tags.Tag         := Tag_Map.To_Tag (Tid);
         P   : aliased Value_Parameters_Type := (null record);
         V   : constant Value_Class_Type     := Constructor (Tag, P'Access);
      begin
         Value := New_Value_Wrapper (V);
         -- Now Value.Ref.all is the prototype of the object.
         Read (Stream'Access, Value.Ref.all);
      end;
   end Read;


   procedure Skip
     (Context : in out Read_Context_Type;
      Block   : in     Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type)
   is
      VW : Value_Wrapper_Type;
   begin
      Read (Context, Block, Cursor, VW);
   end Skip;


   procedure Write
     (Context : in out Write_Context_Type;
      Block   : in out Blocks.Base_Block_Type;
      Cursor  : in out Blocks.Cursor_Type;
      Value   : in     Value_Wrapper_Type)
   is
      pragma Unreferenced (Context);

      Tid    : constant Tag_Map.Tid_Type := Tag_Map.To_Tid (Value.Ref'Tag);
      Stream : aliased Blocks.Streams.Stream_Type :=
         Blocks.Streams.New_Stream (Block'Unrestricted_Access,
                                    Cursor'Unrestricted_Access);
   begin
      Tag_Map.Tid_Type'Write (Stream'Access, Tid);
      Write (Stream'Access, Value.Ref.all);
   end Write;

end DB.Maps.Abstract_Value_Serialization;

