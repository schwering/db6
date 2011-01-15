-- Abstract:
--
-- Root package of different output format implementations.
-- The input is intended to represent the content of a map or, typically, of
-- a list of keys and values.
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Finalization;

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
   -- Returns the next token.

   type Token_Type is
      (Anonymous_Object_Start, 
       Object_Start,
       Object_End,
       Anonymous_Array_Start,
       Array_Start,
       Array_End,
       Value,
       EOF);

   procedure Next_Token
     (Parser  : in out Parser_Type;
      Request : in     AWS.Status.Data;
      Token   :    out Token_Type)
   is abstract;

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

   procedure Value
     (Handler : in out Handler_Type;
      Key     : in     String;
      Value   : in     DB.Maps.Value_Type'Class)
   is abstract;

private
   type String_Ref_Type is access String;

   type Parser_Type is abstract new AF.Limited_Controlled with
      record
         The_Key   : String_Ref_Type;
         The_Value : DB.Maps.Value_Ref_Type;
      end record;

   overriding
   procedure Initialize (Parser : in out Parser_Type);

   overriding
   procedure Finalize (Parser : in out Parser_Type);

   procedure Set_Key
     (Parser : in out Parser_Type;
      Key    : in     String);

   procedure Set_Value
     (Parser : in out Parser_Type;
      Value  : in     String);

   procedure Read
     (Request : in     AWS.Status.Data;
      Buffer  :    out AWS.Status.Stream_Element_Array;
      Last    :    out AWS.Status.Stream_Element_Offset);

end REST.Input_Formats;

