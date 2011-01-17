-- Abstract:
--
-- Deserialization of database entries from JSON format.
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
-- Design Notes:
--
-- XXX There's place for optimization by adding Skip-like Read-procedures
-- instead of calling Next so often.
--
-- Copyright 2008--2011 Christoph Schwering

package REST.Input_Formats.JSON is

   type Parser_Type is new Input_Formats.Parser_Type with private;

   overriding
   function New_Parser return Parser_Type;

   procedure Next_Token
     (Parser  : in out Parser_Type;
      Request : in     AWS.Status.Data;
      Token   :    out Token_Type);

private
   type Parser_Type is new Input_Formats.Parser_Type with null record;

end REST.Input_Formats.JSON;

