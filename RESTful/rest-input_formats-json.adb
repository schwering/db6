-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body REST.Input_Formats.JSON is

   procedure Start_Anonymous_Object (Resource : in out Stream_Type) is
   begin
      null;
   end;


   procedure Start_Object
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      null;
   end;


   procedure End_Object (Resource : in out Stream_Type) is
   begin
      null;
   end;


   procedure Start_Anonymous_Array (Resource : in out Stream_Type) is
   begin
      null;
   end;


   procedure Start_Array
     (Resource : in out Stream_Type;
      Key      : in     String) is
   begin
      null;
   end;


   procedure End_Array (Resource : in out Stream_Type) is
   begin
      null;
   end;


   procedure Put_Value
     (Resource : in out Stream_Type;
      Key      : in     String;
      Value    : in     DB.Maps.Value_Type'Class) is
   begin
      null;
   end;


   procedure Write
     (Resource : in out Stream_Type;
      Buffer   : in     AWS.Status.Stream_Element_Array;
      Last     : in     AWS.Status.Stream_Element_Offset) is
   begin
      null;
   end;

end REST.Input_Formats.JSON;

