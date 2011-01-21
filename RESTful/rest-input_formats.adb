-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;

with DB.Maps.Values;
with DB.Maps.Values.Booleans;
with DB.Maps.Values.Long_Floats;
with DB.Maps.Values.Long_Integers;
with DB.Maps.Values.Nothings;
with DB.Maps.Values.Strings;

with REST.Input_Formats.JSON;

package body REST.Input_Formats is

   function New_Parser (Request : AWS.Status.Data) return Parser_Type'Class is
   begin
      return New_Parser (AWS.Status.Content_Type (Request));
   end New_Parser;


   function New_Parser (Content_Type : String) return Parser_Type'Class is
   begin
      if Content_Type = "application/json" or else
         Content_Type = "application/json" or else
         Content_Type = "application/x-javascript" or else
         Content_Type = "text/javascript" or else
         Content_Type = "text/x-javascript" or else
         Content_Type = "text/x-json"
      then
         return JSON.New_Parser;
--      elsif Content_Type = "application/bson" or else
--            Content_Type = "application/bson" or else
--            Content_Type = "text/x-bson"
--      then
--         return BSON.Parser_Type'(Parser_Type with null record);
      else
         raise Malformed_Input_Data_Error;
      end if;
   end New_Parser;


   procedure Parse
     (Request : in     AWS.Status.Data;
      Parser  : in out Parser_Type'Class;
      Handler : in out Handler_Type'Class) is
   begin
      loop
         declare
            Token : Token_Type;
         begin
            Parser.Next_Token (Request, Token);
            case Token is
               when Object_Start =>
                  if Parser.Has_Key then
                     Handler.Start_Object (Parser.Key);
                  else
                     Handler.Start_Anonymous_Object;
                  end if;
               when Array_Start =>
                  if Parser.Has_Key then
                     Handler.Start_Array (Parser.Key);
                  else
                     Handler.Start_Anonymous_Array;
                  end if;
               when Value =>
                  if Parser.Has_Key then
                     Handler.Value (Parser.Key, Parser.Value);
                  else
                     Handler.Anonymous_Value (Parser.Value);
                  end if;
               when Object_End => Handler.End_Object;
               when Array_End  => Handler.End_Array;
               when EOF        => exit;
               when Error      => Handler.Error;
            end case;
         end;
      end loop;
   end Parse;


   procedure Finalize (Parser : in out Parser_Type)
   is
      use type DB.Maps.Value_Ref_Type;
      procedure Free is new Ada.Unchecked_Deallocation
        (String, String_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (DB.Maps.Value_Type'Class, DB.Maps.Value_Ref_Type);
   begin
      if Parser.The_Key /= null then
         Free (Parser.The_Key);
      end if;
      if Parser.The_Value /= null then
         Free (Parser.The_Value);
      end if;
   end Finalize;


   procedure Set_Key (Parser : in out Parser_Type; Key : in String) is
   begin
      Parser.The_Key := new String'(Key);
   end Set_Key;


   procedure Set_Value (Parser : in out Parser_Type; Value : in String)
   is
      procedure To_Boolean
        (Value   : in  String;
         Bool    : out DB.Maps.Values.Booleans.Value_Type;
         Success : out Boolean) is
      begin
         Bool := DB.Maps.Values.Booleans.New_Value
           (DB.Maps.Values.Booleans.Integer_Type'Value (Value));
         Success := True;
      exception
         when Constraint_Error =>
            Success := False;
      end To_Boolean;

      procedure To_Int
        (Value   : in  String;
         Int     : out DB.Maps.Values.Long_Integers.Value_Type;
         Success : out Boolean) is
      begin
         Int := DB.Maps.Values.Long_Integers.New_Value
           (DB.Maps.Values.Long_Integers.Integer_Type'Value (Value));
         Success := True;
      exception
         when Constraint_Error =>
            Success := False;
      end To_Int;

      procedure To_Float
        (Value   : in  String;
         Flt     : out DB.Maps.Values.Long_Floats.Value_Type;
         Success : out Boolean) is
      begin
         Flt := DB.Maps.Values.Long_Floats.New_Value
           (DB.Maps.Values.Long_Floats.Float_Type'Value (Value));
         Success := True;
      exception
         when Constraint_Error =>
            Success := False;
      end To_Float;

      procedure To_String
        (Value   : in  String;
         Str     : out DB.Maps.Values.Strings.Value_Type;
         Success : out Boolean) is
      begin
         if Value'Length <= 1 or else
            (Value (Value'First) /= '"' or Value (Value'Last) /= '"')
         then
            Success := False;
         else
            Str := DB.Maps.Values.Strings.New_Value
              (Value (Value'First + 1 .. Value'Last - 1));
            Success := True;
         end if;
      end To_String;

      procedure To_Nothing
        (Value   : in  String;
         Success : out Boolean) is
      begin
         Success := Value = "null";
      end To_Nothing;

      function To_Value (Value : String) return DB.Maps.Value_Type'Class
      is
         Success : Boolean;
      begin
         declare
            S : DB.Maps.Values.Strings.Value_Type;
         begin
            To_String (Value, S, Success);
            if Success then
               return S;
            end if;
         end;

         declare
            I : DB.Maps.Values.Long_Integers.Value_Type;
         begin
            To_Int (Value, I, Success);
            if Success then
               return I;
            end if;
         end;

         declare
            F : DB.Maps.Values.Long_Floats.Value_Type;
         begin
            To_Float (Value, F, Success);
            if Success then
               return F;
            end if;
         end;

         declare
            B : DB.Maps.Values.Booleans.Value_Type;
         begin
            To_Boolean (Value, B, Success);
            if Success then
               return B;
            end if;
         end;

         declare
            N : DB.Maps.Values.Nothings.Value_Type;
         begin
            To_Nothing (Value, Success);
            if Success then
               return N;
            end if;
         end;

         raise Malformed_Input_Data_Error;
      end To_Value;

   begin
      Parser.The_Value := new DB.Maps.Value_Type'Class'(To_Value (Value));
   end Set_Value;


   procedure Unset_Key (Parser : in out Parser_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (String, String_Ref_Type);
   begin
      if Parser.The_Key /= null then
         Free (Parser.The_Key);
      end if;
      Parser.The_Key := null;
   end Unset_Key;


   procedure Unset_Value (Parser : in out Parser_Type)
   is
      use type DB.Maps.Value_Ref_Type;
      procedure Free is new Ada.Unchecked_Deallocation
        (DB.Maps.Value_Type'Class, DB.Maps.Value_Ref_Type);
   begin
      if Parser.The_Value /= null then
         Free (Parser.The_Value);
      end if;
      Parser.The_Value := null;
   end Unset_Value;


   function Has_Key (Parser : Parser_Type) return Boolean is
   begin
      return Parser.The_Key /= null;
   end Has_Key;


   function Has_Value (Parser : Parser_Type) return Boolean
   is
      use type DB.Maps.Value_Ref_Type;
   begin
      return Parser.The_Value /= null;
   end Has_Value;


   function Key (Parser : Parser_Type) return String is
   begin
      return Parser.The_Key.all;
   end Key;


   function Value (Parser : Parser_Type) return DB.Maps.Value_Type'Class is
   begin
      return Parser.The_Value.all;
   end Value;


   procedure Fill_Buffer
     (Parser  : in out Parser_Type'Class;
      Request : in     AWS.Status.Data;
      EOF     :    out Boolean) is
   begin
      EOF := False;
      if Parser.Current >= Parser.Last then
         AWS.Status.Read_Body (Request, Parser.Buffer, Parser.Last);
         Parser.Current := Parser.Buffer'First - 1;
         if Parser.Last not in Parser.Buffer'Range then
            EOF := True;
            return;
         end if;
      end if;
   end Fill_Buffer;


   procedure Next
     (Parser  : in out Parser_Type;
      Request : in     AWS.Status.Data;
      Byte    :    out Ada.Streams.Stream_Element;
      EOF     :    out Boolean) is
   begin
      Parser.Fill_Buffer (Request, EOF);
      if not EOF then
         Parser.Current := Parser.Current + 1;
         Byte := Parser.Buffer (Parser.Current);
      end if;
   end Next;


   procedure Next
     (Parser  : in out Parser_Type;
      Request : in     AWS.Status.Data;
      Char    :    out Character;
      EOF     :    out Boolean)
   is
      Byte : Ada.Streams.Stream_Element;
   begin
      Next (Parser, Request, Byte, EOF);
      if not EOF then
         Char := Character'Val (Ada.Streams.Stream_Element'Pos (Byte));
      end if;
   end Next;


   function String_To_Bytes (S : String) return AWS.Status.Stream_Element_Array
   is
      use AWS.Status;
      B : Stream_Element_Array (Stream_Element_Offset (S'First) ..
                                Stream_Element_Offset (S'Last));
   begin
      for I in S'Range loop
         B (Stream_Element_Offset (I)) :=
           Ada.Streams.Stream_Element'Val (Character'Pos (S (I)));
      end loop;
      return B;
   end String_To_Bytes;


   function Bytes_To_String (B : AWS.Status.Stream_Element_Array) return String
   is
      S : String (Positive (B'First) .. Natural (B'Last));
   begin
      for I in B'Range loop
         S (Positive (I)) :=
           Character'Val (Ada.Streams.Stream_Element'Pos (B (I)));
      end loop;
      return S;
   end Bytes_To_String;


   procedure Skip
     (Parser    : in out Parser_Type;
      Request   : in     AWS.Status.Data;
      Byte_List : in     AWS.Status.Stream_Element_Array)
   is
      function Skippable (Byte : Ada.Streams.Stream_Element) return Boolean
      is
         use type Ada.Streams.Stream_Element;
      begin
         for I in Byte_List'Range loop
            if Byte = Byte_List (I) then
               return True;
            end if;
         end loop;
         return False;
      end Skippable;
      pragma Inline (Skippable);
   begin
      Whitespace_Loop:
      loop
         declare
            use type Ada.Streams.Stream_Element;
            EOF : Boolean;
         begin
            Parser.Fill_Buffer (Request, EOF);
            exit when EOF;
            while Parser.Current < Parser.Last loop
               exit Whitespace_Loop
                  when not Skippable (Parser.Buffer (Parser.Current + 1));
               Parser.Current := Parser.Current + 1;
            end loop;
         end;
      end loop Whitespace_Loop;
   end Skip;


   procedure Skip
     (Parser    : in out Parser_Type;
      Request   : in     AWS.Status.Data;
      Char_List : in     String) is
   begin
      Skip (Parser, Request, String_To_Bytes (Char_List));
   end Skip;

end REST.Input_Formats;

