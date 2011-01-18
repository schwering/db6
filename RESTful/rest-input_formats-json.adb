-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Strings.Unbounded;

with REST.Log;

package body REST.Input_Formats.JSON is

   function New_Parser return Parser_Type is
   begin
      return (Input_Formats.Parser_Type with others => <>);
   end New_Parser;


   Whitespace : constant String := (' ', ASCII.HT, ASCII.LF, ASCII.CR, ',');

   procedure Next_Token
     (Parser  : in out Parser_Type;
      Request : in     AWS.Status.Data;
      Token   :    out Token_Type)
   is
      -- The Parse* functions are allowed to re-use Char. They may also write
      -- Char and EOF for their own purposes, because Char and EOF aren't used
      -- after that.
      -- The Parse* functions need to set Token in case an Error occurred.
      -- For that reason, in the case-statement the Token needs to be set before
      -- Parse* is called.

      Char : Character;
      EOF  : Boolean;

      function Parse_String (Quotes : Boolean := True) return String
      is
         use Ada.Strings.Unbounded;
         T       : Unbounded_String;
         Escaped : Boolean := False;
      begin
         if Quotes then
            Append (T, '"');
         end if;
         loop
            Next (Parser, Request, Char, EOF);
            exit when (not Escaped and (Char = ''' or Char = '"')) or EOF;
            Escaped := not Escaped and Char = '\';
            Append (T, Char);
         end loop;
         if Quotes then
            Append (T, '"');
         end if;
         if EOF then
            Token := Error;
            REST.Log.Info ("JSON: EOF during String");
         end if;
         return To_String (T);
      end Parse_String;

      function Parse_Number return String
      is
         use Ada.Strings.Unbounded;
         T : Unbounded_String;
      begin
         loop
            exit when (Char /= '-' and
                       Char /= '.' and
                       Char not in '0' .. '9') or
                      EOF;
            Append (T, Char);
            Next (Parser, Request, Char, EOF);
         end loop;
         if EOF then
            Token := Error;
            REST.Log.Info ("JSON: EOF during Number");
         end if;
         return To_String (T);
      end Parse_Number;

      function Parse (S : String) return String
      is
         T : String (S'Range);
      begin
         if S'Length = 0 then
            return "";
         end if;
         T (T'First) := Char;
         for I in S'First + 1 .. S'Last loop
            Next (Parser, Request, T (I), EOF);
            if EOF then
               Token := Error;
               REST.Log.Info ("JSON: EOF during keyword");
               return T (T'First .. I);
            end if;
         end loop;
         return T;
      end Parse;

   begin
      Skip (Parser, Request, Whitespace);
      Next (Parser, Request, Char, EOF);
      if EOF then
         Token := Input_Formats.EOF;
         goto EOF_Handling;
      end if;
      case Char is
         when '{' =>
            if Parser.Level = Parser.Is_Array'Last then
               Token := Error;
               REST.Log.Info ("JSON: reached indentation level");
            else
               Token := Object_Start;
               Parser.Level := Parser.Level + 1;
               Parser.Is_Array (Parser.Level) := False;
               Parser.Expect_Key := True;
            end if;
         when '}' =>
            if Parser.Level = 0 then
               Token := Error;
               REST.Log.Info ("JSON: too many un-indentations");
            else
               Token := Object_End;
               Parser.Level := Parser.Level - 1;
               Parser.Expect_Key := True;
            end if;
         when '[' =>
            if Parser.Level = Parser.Is_Array'Last then
               Token := Error;
               REST.Log.Info ("JSON: reached indentation level");
            else
               Token := Array_Start;
               if Parser.Is_Array (Parser.Level) then
                  Parser.Unset_Key;
               end if;
               Parser.Level := Parser.Level + 1;
               Parser.Is_Array (Parser.Level) := True;
               Parser.Expect_Key := True;
            end if;
         when ']' =>
            if Parser.Level = 0 then
               Token := Error;
               REST.Log.Info ("JSON: too many un-indentations");
            else
               Token := Array_End;
               Parser.Level := Parser.Level - 1;
               Parser.Expect_Key := True;
            end if;
         when '"' | ''' =>
            if Parser.Level not in Parser.Is_Array'Range then
               Token := Error;
               REST.Log.Info ("JSON: indentation level out of range");
            elsif not Parser.Is_Array (Parser.Level) and Parser.Expect_Key then
               Parser.Expect_Key := False;
               Parser.Set_Key (Parse_String (Quotes => False));
               Skip (Parser, Request, Whitespace);
               Next (Parser, Request, Char, EOF);
               if EOF or else Char /= ':' then
                  Token := Error;
                  REST.Log.Info ("JSON: key/value delimiter missing");
                  goto EOF_Handling;
               end if;
               Parser.Unset_Value;
               Parser.Next_Token (Request, Token);
            else
               Token := Value;
               if Parser.Is_Array (Parser.Level) then
                  Parser.Unset_Key;
               end if;
               Parser.Expect_Key := True;
               Parser.Set_Value (Parse_String);
            end if;
         when '-' | '.' | '0' .. '9' =>
            if Parser.Level not in Parser.Is_Array'Range then
               Token := Error;
               REST.Log.Info ("JSON: indentation level out of range");
            elsif Parser.Expect_Key then
               Token := Error;
               REST.Log.Info ("JSON: expecting a key, found a number");
            else
               Token := Value;
               Parser.Expect_Key := True;
               Parser.Set_Value (Parse_Number);
            end if;
         when 't' =>
            if Parser.Level not in Parser.Is_Array'Range then
               Token := Error;
               REST.Log.Info ("JSON: indentation level out of range");
            elsif Parser.Expect_Key then
               Token := Error;
               REST.Log.Info ("JSON: expecting a key, found a true");
            else
               Token := Value;
               Parser.Expect_Key := True;
               Parser.Set_Value (Parse ("true"));
            end if;
         when 'f' =>
            if Parser.Level not in Parser.Is_Array'Range then
               Token := Error;
               REST.Log.Info ("JSON: indentation level out of range");
            elsif Parser.Expect_Key then
               Token := Error;
               REST.Log.Info ("JSON: expecting a key, found a false");
            else
               Token := Value;
               Parser.Expect_Key := True;
               Parser.Set_Value (Parse ("false"));
            end if;
         when 'n' =>
            if Parser.Level not in Parser.Is_Array'Range then
               Token := Error;
               REST.Log.Info ("JSON: indentation level out of range");
            elsif Parser.Expect_Key then
               Token := Error;
               REST.Log.Info ("JSON: expecting a key, found a null");
            else
               Token := Value;
               Parser.Expect_Key := True;
               Parser.Set_Value (Parse ("null"));
            end if;
         when others =>
            Token := Error;
            REST.Log.Info ("JSON: invalid character '"& Char &"'");
      end case;

      <<EOF_Handling>>
      if Token = Input_Formats.EOF and Parser.Level /= 0 then
         Token := Error;
         REST.Log.Info ("JSON: caught EOF at indentation level "&
                        Natural'Image (Parser.Level));
      end if;
   end Next_Token;

end REST.Input_Formats.JSON;

