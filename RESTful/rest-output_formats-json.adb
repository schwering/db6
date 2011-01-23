-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Characters.Handling;

with DB.Types.Values;

package body REST.Output_Formats.JSON is

   function Content_Type (Writer : Writer_Type) return String
   is
      pragma Unreferenced (Writer);
   begin
      return "application/json";
   end Content_Type;


   procedure Emit
     (Resource : in out Writer_Type;
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


   procedure Indent (Resource : in out Writer_Type) is
      Whitespace : constant String (1 .. Resource.Indent * 3 + 1) :=
        (1 => ASCII.LF, others => ' ');
   begin
      Emit (Resource, Whitespace);
   end Indent;


   function Escape (S : String) return String
   is
      N : Natural := 0;
   begin
      for I in S'Range loop
         if S (I) = '\' or S (I) = '"' then
            N := N + 1;
         end if;
      end loop;
      if N = 0 then
         return S;
      else
         declare
            T : String (S'First .. S'Last + N);
         begin
            for I in reverse S'Range loop
               T (I + N) := S (I);
               if S (I) = '\' or S (I) = '"' then
                  N := N - 1;
                  T (I + N) := '\';
               end if;
            end loop;
            return T;
         end;
      end if;
   end Escape;


   procedure Start_Anonymous_Object (Resource : in out Writer_Type) is
   begin
      if Resource.Comma then
         Emit (Resource, ",");
      end if;
      --Indent (Resource);
      Emit (Resource, "{");
      Resource.Indent := Resource.Indent + 1;
      Resource.Comma  := False;
   end Start_Anonymous_Object;


   procedure Start_Object
     (Resource : in out Writer_Type;
      Key      : in     String) is
   begin
      if Resource.Comma then
         Emit (Resource, ",");
      end if;
      Indent (Resource);
      Emit (Resource, """");
      Emit (Resource, Escape (Key));
      Emit (Resource, """ : ");
      Emit (Resource, "{");
      Resource.Indent := Resource.Indent + 1;
      Resource.Comma  := False;
   end Start_Object;


   procedure End_Object (Resource : in out Writer_Type) is
   begin
      Resource.Indent := Resource.Indent - 1;
      Indent (Resource);
      Emit (Resource, "}");
      Resource.Comma := True;
   end End_Object;


   procedure Start_Anonymous_Array (Resource : in out Writer_Type) is
   begin
      if Resource.Comma then
         Emit (Resource, ",");
      end if;
      --Indent (Resource);
      Emit (Resource, "[");
      Resource.Indent := Resource.Indent + 1;
      Resource.Comma  := False;
   end Start_Anonymous_Array;


   procedure Start_Array
     (Resource : in out Writer_Type;
      Key      : in     String) is
   begin
      if Resource.Comma then
         Emit (Resource, ",");
      end if;
      Indent (Resource);
      Emit (Resource, """");
      Emit (Resource, Escape (Key));
      Emit (Resource, """ : ");
      Emit (Resource, "[");
      Resource.Indent := Resource.Indent + 1;
      Resource.Comma := False;
   end Start_Array;


   procedure End_Array (Resource : in out Writer_Type) is
   begin
   Resource.Indent := Resource.Indent - 1;
   Indent (Resource);
   Emit (Resource, "]");
   Resource.Comma := True;
   end End_Array;


   procedure Put_Value
     (Resource : in out Writer_Type;
      Key      : in     String;
      Value    : in     DB.Types.Values.Value_Type)
   is
      use type DB.Types.Values.Tag_Type;
   begin
      if Resource.Comma then
         Emit (Resource, ",");
      end if;
      Indent (Resource);
      Emit (Resource, """");
      Emit (Resource, Escape (Key));
      Emit (Resource, """ : ");
      if Value.Tag = DB.Types.Values.Bounded_String_Value or
         Value.Tag = DB.Types.Values.Unbounded_String_Value
      then
         Emit (Resource, """");
         Emit (Resource, Escape (DB.Types.Values.Image (Value)));
         Emit (Resource, """");
      elsif Value.Tag = DB.Types.Values.Boolean_Value then
         Emit (Resource, Ada.Characters.Handling.To_Lower (DB.Types.Values.Image
           (Value)));
      else
         Emit (Resource, DB.Types.Values.Image (Value));
      end if;
      Resource.Comma := True;
   end Put_Value;


   procedure Put_Anonymous_Value
     (Resource : in out Writer_Type;
      Value    : in     DB.Types.Values.Value_Type)
   is
      use type DB.Types.Values.Tag_Type;
   begin
      if Resource.Comma then
         Emit (Resource, ",");
      end if;
      Indent (Resource);
      if Value.Tag = DB.Types.Values.Bounded_String_Value or
         Value.Tag = DB.Types.Values.Unbounded_String_Value
      then
         Emit (Resource, """");
         Emit (Resource, Escape (DB.Types.Values.Image (Value)));
         Emit (Resource, """");
      elsif Value.Tag =  DB.Types.Values.Boolean_Value then
         Emit (Resource, Ada.Characters.Handling.To_Lower (DB.Types.Values.Image
           (Value)));
      else
         Emit (Resource, DB.Types.Values.Image (Value));
      end if;
      Resource.Comma := True;
   end Put_Anonymous_Value;

end REST.Output_Formats.JSON;

