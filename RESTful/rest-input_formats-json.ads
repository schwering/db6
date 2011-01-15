-- Abstract:
--
-- Serialization of database entries to JSON format.
--
-- Copyright 2008--2011 Christoph Schwering

package REST.Input_Formats.JSON is

   type Parser_Type is new Input_Formats.Parser_Type with private;

   procedure Next_Token
     (Parser  : in out Parser_Type;
      Request : in     AWS.Status.Data;
      Token   :    out Token_Type);

private
   type Parser_Type is new Input_Formats.Parser_Type with null record;

end REST.Input_Formats.JSON;

