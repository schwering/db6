-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Maps.Values.Strings;

package body REST.Output_Formats.JSON is

   function Content_Type (Stream : Stream_Type) return String is
   begin
      return "application/json";
   end Content_Type;


   procedure Emit
     (Resource : in out Stream_Type;
      Str      : in     String)
   is
      pragma Assert (String'Component_Size =
                     Queues.Item_Array_Type'Component_Size); 
      Buf : Queues.Item_Array_Type (Str'Range);
      for Buf'Address use Str'Address;
      Last : Natural;
   begin
      pragma Assert (Str'Size = Buf'Size);
      loop
         Queues.Enqueue (Resource.Queue, Buf, Last);
         exit when Last = Buf'Last;
      end loop;
   end Emit;


   procedure Indent (Resource : in out Stream_Type) is
      Whitespace : constant String (1 .. Resource.Indent) :=
        (others => ' ');
   begin
      Emit (Resource, Whitespace);
   end Indent;


   procedure Start_Anonymous_Object (Resource : in out Stream_Type) is
   begin
      Indent (Resource);
      Emit (Resource, "{");
      Resource.Indent := Resource.Indent + 1;
   end Start_Anonymous_Object;


   procedure Start_Object
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Indent (Resource);
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """ : ");
      Emit (Resource, "{");
      Resource.Indent := Resource.Indent + 1;
   end Start_Object;


   procedure End_Object (Resource : in out Stream_Type) is
   begin
      Resource.Indent := Resource.Indent - 1;
      Indent (Resource);
      Emit (Resource, "},");
   end End_Object;


   procedure Start_Anonymous_Array (Resource : in out Stream_Type) is
   begin
      Indent (Resource);
      Emit (Resource, "[");
      Resource.Indent := Resource.Indent + 1;
   end Start_Anonymous_Array;


   procedure Start_Array
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Indent (Resource);
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """ : ");
      Emit (Resource, "[");
      Resource.Indent := Resource.Indent + 1;
   end Start_Array;


   procedure End_Array (Resource : in out Stream_Type) is
   begin
      Resource.Indent := Resource.Indent - 1;
      Indent (Resource);
      Emit (Resource, "],");
   end End_Array;


   procedure Put_Value
     (Resource : in out Stream_Type;
      Key      : in     String;
      Value    : in     DB.Maps.Value_Type'Class) is
   begin
      Indent (Resource);
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """ : ");
      if Value in DB.Maps.Values.Strings.Value_Type'Class then
         Emit (Resource, """");
         Emit (Resource, Value.Image);
         Emit (Resource, """");
      else
         Emit (Resource, Value.Image);
      end if;
      Emit (Resource, ",");
      Resource.Indent := Resource.Indent + 1;
   end Put_Value;

end REST.Output_Formats.JSON;

