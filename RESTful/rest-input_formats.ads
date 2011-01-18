-- Abstract:
--
-- Root package of different output format implementations.
-- The input is intended to represent the content of a map or, typically, of
-- a list of keys and values.
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Finalization;
with Ada.Streams;

with AWS.Status;

with DB.Maps;

package REST.Input_Formats is

   package AF renames Ada.Finalization;

   type Parser_Type is abstract tagged limited private;

   type Handler_Type is limited interface;

   procedure Parse
     (Request : in     AWS.Status.Data;
      Parser  : in out Parser_Type'Class;
      Handler : in out Handler_Type'Class);

   ----------
   -- Parser operations.

   function New_Parser (Content_Type : String) return Parser_Type'Class;

   function New_Parser return Parser_Type
   is abstract;

   type Token_Type is
      (Object_Start,
       Object_End,
       Array_Start,
       Array_End,
       Value,
       EOF,
       Error);

   procedure Next_Token
     (Parser  : in out Parser_Type;
      Request : in     AWS.Status.Data;
      Token   :    out Token_Type)
   is abstract;

   function Has_Key (Parser : Parser_Type) return Boolean;
   function Has_Value (Parser : Parser_Type) return Boolean;

   function Key (Parser : Parser_Type) return String;
   function Value (Parser : Parser_Type) return DB.Maps.Value_Type'Class;

   ----------
   -- Callbacks to process data from the parser.

   procedure Start_Anonymous_Object (Handler : in out Handler_Type)
   is abstract;

   procedure Start_Object
     (Handler : in out Handler_Type;
      Key     : in     String)
   is abstract;

   procedure End_Object (Handler : in out Handler_Type)
   is abstract;

   procedure Start_Anonymous_Array (Handler : in out Handler_Type)
   is abstract;

   procedure Start_Array
     (Handler : in out Handler_Type;
      Key     : in     String)
   is abstract;

   procedure End_Array (Handler : in out Handler_Type)
   is abstract;

   procedure Anonymous_Value
     (Handler : in out Handler_Type;
      Value   : in     DB.Maps.Value_Type'Class)
   is abstract;

   procedure Value
     (Handler : in out Handler_Type;
      Key     : in     String;
      Value   : in     DB.Maps.Value_Type'Class)
   is abstract;

   procedure Error (Handler : in out Handler_Type)
   is abstract;

private
   type String_Ref_Type is access String;

   subtype Buffer_Type is AWS.Status.Stream_Element_Array (1 .. 128);

   use type AWS.Status.Stream_Element_Offset;

   type Parser_Type is abstract new AF.Limited_Controlled with
      record
         Buffer     : Buffer_Type;
         Last       : AWS.Status.Stream_Element_Offset := Buffer_Type'First - 1;
         Current    : AWS.Status.Stream_Element_Offset := Buffer_Type'First - 1;
         The_Key    : String_Ref_Type                  := null;
         The_Value  : DB.Maps.Value_Ref_Type           := null;
         Expect_Key : Boolean                          := True;
      end record;

   overriding
   procedure Finalize (Parser : in out Parser_Type);

   procedure Set_Key
     (Parser : in out Parser_Type;
      Key    : in     String);

   procedure Set_Value
     (Parser : in out Parser_Type;
      Value  : in     String);

   procedure Unset_Key (Parser : in out Parser_Type);

   procedure Unset_Value (Parser : in out Parser_Type);

   procedure Fill_Buffer
     (Parser  : in out Parser_Type'Class;
      Request : in     AWS.Status.Data;
      EOF     :    out Boolean);

   procedure Next
     (Parser  : in out Parser_Type;
      Request : in     AWS.Status.Data;
      Byte    :    out Ada.Streams.Stream_Element;
      EOF     :    out Boolean);

   procedure Next
     (Parser  : in out Parser_Type;
      Request : in     AWS.Status.Data;
      Char    :    out Character;
      EOF     :    out Boolean);

   function String_To_Bytes (S : String) return AWS.Status.Stream_Element_Array;
   function Bytes_To_String (B : AWS.Status.Stream_Element_Array) return String;

   procedure Skip
     (Parser    : in out Parser_Type;
      Request   : in     AWS.Status.Data;
      Byte_List : in     AWS.Status.Stream_Element_Array);

   procedure Skip
     (Parser    : in out Parser_Type;
      Request   : in     AWS.Status.Data;
      Char_List : in     String);

   pragma Inline (Next);
   pragma Inline (String_To_Bytes);

end REST.Input_Formats;

