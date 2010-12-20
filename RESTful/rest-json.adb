-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body REST.JSON is

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


   procedure Start_JSON
     (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "{");
   end Start_JSON;


   procedure End_JSON
     (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "}");
      Queues.Mark_Final (Resource.Queue);
   end End_JSON;


   procedure Put_Float
     (Resource : in out Stream_Type;
      Key      : in     String;
      Float    : in     Float_Value_Type) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, Float.Image);
      Emit (Resource, ",");
   end Put_Float;


   procedure Put_Integer
     (Resource : in out Stream_Type;
      Key      : in     String;
      Integer  : in     Integer_Value_Type) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, Integer.Image);
      Emit (Resource, ",");
   end Put_Integer;


   procedure Put_String
     (Resource : in out Stream_Type;
      Key      : in     String;
      Str      : in     String_Value_Type) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=""");
      Emit (Resource, Str.Image);
      Emit (Resource, """,");
   end Put_String;


   procedure Put_Null
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, "null");
      Emit (Resource, ",");
   end Put_Null;


   procedure Put_Object
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, "{");
   end Put_Object;


   procedure End_Object (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "},");
   end End_Object;


   procedure Put_Array
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, "[");
   end Put_Array;


   procedure End_Array (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "],");
   end End_Array;


   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out AS.Stream_Element_Array;
      Last     :    out AS.Stream_Element_Offset)
   is
      pragma Assert (Buffer'Component_Size =
                     Queues.Item_Array_Type'Component_Size); 
      use type AS.Stream_Element_Offset;
      Q_Buf : Queues.Item_Array_Type
        (Natural (Buffer'First) .. Natural (Buffer'Last));
      for Q_Buf'Address use Buffer'Address;
      Q_Last : Natural;
   begin
      pragma Assert (Buffer'Size = Q_Buf'Size);
      if Buffer'Length = 0 then
         Last := Buffer'First - 1;
         return;
      end if;

      Queues.Dequeue (Resource.Queue, Q_Buf, Q_Last);
      Last := AS.Stream_Element_Offset (Q_Last);
   end Read;


   procedure Close (Resource : in out Stream_Type) is
   begin
      Queues.Mark_Final (Resource.Queue);
   end Close;


   function End_Of_File (Resource : Stream_Type) return Boolean is
   begin
      return Queues.Is_Final (Resource.Queue);
   end End_Of_File;

end REST.JSON;

