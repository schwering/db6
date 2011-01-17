-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Strings.Unbounded;

package body REST.Input_Formats.JSON is

   function New_Parser return Parser_Type is
   begin
      return (Input_Formats.Parser_Type with null record);
   end New_Parser;


   Whitespace : constant String := (' ', ASCII.HT, ASCII.LF, ASCII.CR);

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

      function Parse_String return String
      is
         use Ada.Strings.Unbounded;
         T       : Unbounded_String := To_Unbounded_String (32);
         Escaped : Boolean := False;
      begin
         Append (T, '"');
         loop
            Next (Parser, Request, Char, EOF);
            exit when (not Escaped and (Char = ''' or Char = '"')) or EOF;
            Escaped := not Escaped and Char = '\';
            Append (T, Char);
         end loop;
         Append (T, '"');
         if EOF then
            Token := Error;
         end if;
         return To_String (T);
      end Parse_String;

      function Parse_Number return String
      is
         use Ada.Strings.Unbounded;
         T : Unbounded_String := To_Unbounded_String (8);
      begin
         loop
            Next (Parser, Request, Char, EOF);
            exit when Char = '-' or Char = '.' or Char in '0' .. '9' or EOF;
            Append (T, Char);
         end loop;
         if EOF then
            Token := Error;
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
         return;
      end if;
      case Char is
         when '{' => Token := Object_Start;
         when '}' => Token := Object_End;
         when '[' => Token := Array_Start;
         when ']' => Token := Array_End;
         when '"' | ''' =>
            Token := Value;
            Parser.Set_Key (Parse_String);
         when '-' | '.' | '0' .. '9' =>
            Token := Value;
            Parser.Set_Key (Parse_Number);
         when 't' =>
            Token := Value;
            Parser.Set_Key (Parse ("true"));
         when 'f' =>
            Token := Value;
            Parser.Set_Key (Parse ("false"));
         when 'n' =>
            Token := Value;
            Parser.Set_Key (Parse ("null"));
         when others =>
            Token := Error;
      end case;
   end Next_Token;

end REST.Input_Formats.JSON;

