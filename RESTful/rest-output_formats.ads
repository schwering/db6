-- Abstract:
--
-- Root package of different output format implementations.
-- The output is intended to represent the content of a map or, typically, of
-- the keys and values traversed by a cursor.
--
-- A natural format of the output is JSON. The format of a cursor in JSON would
-- look like this:
-- [
--   "peter" : {
--     "lastname" : "mueller",
--     "age" : 16
--   },
--   "klara" : {
--     "lastname" : "zylinder",
--     "age" : 105
--   },
--   ...
-- ]
--
-- By the way: Even though it seems not to be documentated, AWS frees the stream
-- that is handed over to AWS.Response.Build, at least if Server_Close is set to
-- True.
--
-- Design Notes:
--
-- We have a consumer-producer-scenario here:
--
-- One thread iterates the cursor and populates the stream with the content from
-- the cursor using the Populate_From_Cursor procedure. He's the producer.
--
-- Another thread reads the stream using the Read procedure. He's the consumer.
-- The code that actually calls Read is usually somewhere in the AWS code: when
-- building the HTTP response, the stream is handed over to AWS.Response.Build
-- and when AWS really responds, it reads the stream.
--
-- For performance and memory reasons, it is important that the stream is not
-- simply a big heap which is first populated and then read at once. Instead,
-- the stream has a internal queue and items are enqueued and dequeued
-- alternately.
--
-- This, of course, implies that the producer task is blocked in general until
-- the consumer has read (or closed) the stream. (Unless the answer is so small
-- that it fits in the queue's buffer at once, of course.)
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Resources.Streams;

with DB.Maps;
with DB.Types.Values;
private with DB.DSA.Utils.Gen_Queues;

with REST.Maps;
private with REST.Maps.Cursors;

package REST.Output_Formats is

   use Ada.Strings.Unbounded;
   package AS renames Ada.Streams;
   package ARS renames AWS.Resources.Streams;

   type Writer_Type is abstract new ARS.Stream_Type with private;

   type Writer_Ref_Type is access Writer_Type'Class;

   ----------
   -- Populates a stream with objects from the cursor.

   procedure Initialize_Writer
     (Writer            : in Writer_Ref_Type;
      Map               : in REST.Maps.Map_Ref_Type;
      URL_Path          : in Unbounded_String;
      Offset            : in Natural;
      Lower_Bound       : in DB.Maps.Bound_Type;
      Upper_Bound       : in DB.Maps.Bound_Type;
      Has_Column_Regexp : in Boolean;
      Column_Regexp     : in String;
      Max_Objects       : in Natural;
      Next_URL          : in Unbounded_String);
   -- Populates the stream with at most Max_Objects from the cursor created from
   -- the given Map with Lower_Bound and Upper_Bound and Column_Regexp.
   -- Object is meant in the JSON-sense and not key/value-pairs returned from
   -- cursor.

   ----------
   -- Information about the stream.

   function Content_Type (Writer : Writer_Type) return String
   is abstract;

   ----------
   -- Operations to manipulate the content of the stream.

   procedure Start_Anonymous_Object (Resource : in out Writer_Type)
   is abstract;

   procedure Start_Object
     (Resource : in out Writer_Type;
      Key      : in     String)
   is abstract;

   procedure End_Object (Resource : in out Writer_Type)
   is abstract;

   procedure Start_Anonymous_Array (Resource : in out Writer_Type)
   is abstract;

   procedure Start_Array
     (Resource : in out Writer_Type;
      Key      : in     String)
   is abstract;

   procedure End_Array (Resource : in out Writer_Type)
   is abstract;

   procedure Put_Value
     (Resource : in out Writer_Type;
      Key      : in     String;
      Value    : in     DB.Types.Values.Value_Type)
   is abstract;

   procedure Put_Anonymous_Value
     (Resource : in out Writer_Type;
      Value    : in     DB.Types.Values.Value_Type)
   is abstract;

   ----------
   -- Operations to read the stream.

   procedure Read
     (Resource : in out Writer_Type;
      Buffer   :    out AS.Stream_Element_Array;
      Last     :    out AS.Stream_Element_Offset);

   procedure Close (Resource : in out Writer_Type);

   function End_Of_File (Resource : Writer_Type) return Boolean;

   procedure Reset (Resource : in out Writer_Type) is null;

   procedure Set_Index
     (Resource : in out Writer_Type;
      To       : in     AS.Stream_Element_Offset) is null;

private
   package Queues is new DB.DSA.Utils.Gen_Queues
     (Queue_Size => 1023,
      Item_Type  => AS.Stream_Element);

   task type Populator_Type is
      entry Initialize
        (Writer      : in Writer_Ref_Type;
         Max_Objects : in Natural;
         Next_URL    : in Unbounded_String);
      entry Stop;
   end Populator_Type;

   type Writer_Type is abstract new ARS.Stream_Type with
      record
         Queue         : Queues.Queue_Type;
         Cursor        : Maps.Cursors.Cursor_Ref_Type;
         Populator     : Populator_Type;
         Self          : Writer_Ref_Type := null;
         Initialized   : Boolean := False;
      end record;

end REST.Output_Formats;

