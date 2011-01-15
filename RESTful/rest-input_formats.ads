-- Abstract:
--
-- Root package of different output format implementations.
-- The input is intended to represent the content of a map or, typically, of
-- a list of keys and values.
--
-- Copyright 2008--2011 Christoph Schwering

with AWS.Status;

with DB.Maps.Values;

package REST.Input_Formats is

   type Stream_Type is abstract tagged null record;

   procedure Parser
     (Resource : in out Stream_Type;
      Request  : in     AWS.Status.Data);

   ----------
   -- Callbacks to process data from the stream.

   procedure Start_Anonymous_Object (Resource : in out Stream_Type)
   is abstract;

   procedure Start_Object
     (Resource : in out Stream_Type;
      Key      : in     String)
   is abstract;

   procedure End_Object (Resource : in out Stream_Type)
   is abstract;

   procedure Start_Anonymous_Array (Resource : in out Stream_Type)
   is abstract;

   procedure Start_Array
     (Resource : in out Stream_Type;
      Key      : in     String)
   is abstract;

   procedure End_Array (Resource : in out Stream_Type)
   is abstract;

   procedure Put_Value
     (Resource : in out Stream_Type;
      Key      : in     String;
      Value    : in     DB.Maps.Value_Type'Class)
   is abstract;

   ----------
   -- Operations to write the stream.

   procedure Write
     (Resource : in out Stream_Type;
      Buffer   : in     AWS.Status.Stream_Element_Array;
      Last     : in     AWS.Status.Stream_Element_Offset)
   is abstract;

   procedure Close (Resource : in out Stream_Type)
   is null;

end REST.Input_Formats;

