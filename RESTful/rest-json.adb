-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Maps.Values.Strings;

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


   procedure Start_Anonymous_Object (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "{");
   end Start_Anonymous_Object;


   procedure Start_Object
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, "{");
   end Start_Object;


   procedure End_Object (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "},");
   end End_Object;


   procedure Start_Anonymous_Array (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "[");
   end Start_Anonymous_Array;


   procedure Start_Array
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      Emit (Resource, "[");
   end Start_Array;


   procedure End_Array (Resource : in out Stream_Type) is
   begin
      Emit (Resource, "],");
   end End_Array;


   procedure Put_Value
     (Resource : in out Stream_Type;
      Key      : in     String;
      Value    : in     DB.Maps.Value_Type'Class) is
   begin
      Emit (Resource, """");
      Emit (Resource, Key);
      Emit (Resource, """=");
      if Value in DB.Maps.Values.Strings.Value_Type'Class then
         Emit (Resource, """");
         Emit (Resource, Value.Image);
         Emit (Resource, """");
      else
         Emit (Resource, Value.Image);
      end if;
      Emit (Resource, ",");
   end Put_Value;


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

