-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

package body REST.Input_Formats.JSON is

   procedure Next_Token
     (Parser  : in out Parser_Type;
      Request : in     AWS.Status.Data;
      Token   :    out Token_Type)
   is
   begin
      null;
   end Next_Token;

end REST.Input_Formats.JSON;

