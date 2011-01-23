-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Tags;
with Ada.Unchecked_Deallocation;

with AWS.Status.Set;

with AUnit.Assertions; use AUnit.Assertions;

with DB.Types.Values;

package body REST.Input_Formats.JSON.Test is

   overriding
   procedure Set_Up (T : in out Test_Type) is
   begin
      T.P := new Parser_Type'(New_Parser);
   end Set_Up;


   overriding
   procedure Tear_Down (T : in out Test_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Parser_Type, Parser_Ref_Type);
   begin
      Free (T.P);
   end Tear_Down;


   type Handler_Type (Length : Natural) is new Input_Formats.Handler_Type with
      record
         Expected : Item_Array_Type (1 .. Length);
         I        : Natural := 0;
      end record;

   overriding
   procedure Start_Anonymous_Object (Handler : in out Handler_Type);

   overriding
   procedure Start_Object
     (Handler : in out Handler_Type;
      Key     : in     String);

   overriding
   procedure End_Object (Handler : in out Handler_Type);

   overriding
   procedure Start_Anonymous_Array (Handler : in out Handler_Type);

   overriding
   procedure Start_Array
     (Handler : in out Handler_Type;
      Key     : in     String);

   overriding
   procedure End_Array (Handler : in out Handler_Type);

   overriding
   procedure Anonymous_Value
     (Handler : in out Handler_Type;
      Val     : in     DB.Types.Values.Value_Type);

   overriding
   procedure Value
     (Handler : in out Handler_Type;
      Key     : in     String;
      Val     : in     DB.Types.Values.Value_Type);

   overriding
   procedure Error (Handler : in out Handler_Type);


   procedure Start_Anonymous_Object (Handler : in out Handler_Type) is
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Anonymous_Object_Start,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got anonymous object start");
   end Start_Anonymous_Object;


   procedure Start_Object
     (Handler : in out Handler_Type;
      Key     : in     String) is
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Object_Start,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got object start");
      Assert (To_String (Handler.Expected (Handler.I).Key) = Key,
              "expected '"& To_String (Handler.Expected (Handler.I).Key) &"', "&
              "got '"& Key &"'");
   end Start_Object;


   procedure End_Object (Handler : in out Handler_Type) is
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Object_End,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got object end");
   end End_Object;


   procedure Start_Anonymous_Array (Handler : in out Handler_Type) is
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Anonymous_Array_Start,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got anonymous array start");
   end Start_Anonymous_Array;


   procedure Start_Array
     (Handler : in out Handler_Type;
      Key     : in     String) is
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Array_Start,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got array start");
      Assert (To_String (Handler.Expected (Handler.I).Key) = Key,
              "expected '"& To_String (Handler.Expected (Handler.I).Key) &"', "&
              "got '"& Key &"'");
   end Start_Array;


   procedure End_Array (Handler : in out Handler_Type) is
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Array_End,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got array end");
   end End_Array;


   procedure Anonymous_Value
     (Handler : in out Handler_Type;
      Val     : in     DB.Types.Values.Value_Type)
   is
      use type Ada.Tags.Tag;
      use type DB.Types.Values.Value_Type;
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Anonymous_Value,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got value");
      Assert (Handler.Expected (Handler.I).Value = Val,
              "expected '"& DB.Types.Values.Image (Handler.Expected (Handler.I).Value)
              &"', "& "got '"& DB.Types.Values.Image (Val) &"'");
   end Anonymous_Value;


   procedure Value
     (Handler : in out Handler_Type;
      Key     : in     String;
      Val     : in     DB.Types.Values.Value_Type)
   is
      use type Ada.Tags.Tag;
      use type DB.Types.Values.Value_Type;
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Value,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got value");
      Assert (To_String (Handler.Expected (Handler.I).Key) = Key,
              "expected '"& To_String (Handler.Expected (Handler.I).Key) &"', "&
              "got '"& Key &"'");
      Assert (Handler.Expected (Handler.I).Value = Val,
              "expected '"& DB.Types.Values.Image (Handler.Expected (Handler.I).Value)
              &"', "& "got '"& DB.Types.Values.Image (Val) &"'");
   end Value;


   procedure Error (Handler : in out Handler_Type) is
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Error,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got error");
   end Error;


   function TUS (S : String) return Unbounded_String
   renames To_Unbounded_String;


   procedure Init_Body (Request : in out AWS.Status.Data; S : in String) is
   begin
      AWS.Status.Set.Append_Body (Request, String_To_Bytes (S));
   end Init_Body;


   procedure Test_Flat (T : in out Test_Type)
   is
      use DB.Types.Values;
      Expected : constant Item_Array_Type :=
        ((Event => Anonymous_Object_Start),
         (Value, TUS ("key1"), New_Value ("string1")),
         (Value, TUS ("key2"), New_Value ("string2")),
         (Value, TUS ("key3"), New_Value ("string3")),
         (Value, TUS ("key4"), New_Value (DB.Types.Values.Long_Integers.Discrete_Type (123))),
         (Value, TUS ("key5"), New_Value (DB.Types.Values.Long_Integers.Discrete_Type (-123))),
         (Value, TUS ("key6"), New_Value (DB.Types.Values.Long_Reals.Real_Type (123.901239))),
         (Value, TUS ("key7"), New_Value (DB.Types.Values.Long_Reals.Real_Type (-123.0))),
         (Value, TUS ("key8"), New_Value (False)),
         (Value, TUS ("key9"), New_Value (True)),
         (Value, TUS ("key10"), Nothing_Value),
         (Event => Object_End));
      Request : AWS.Status.Data;
      Handler : Handler_Type := (Expected'Length, Expected, others => <>);
   begin
      Init_Body
        (Request,
         "{"&
         "   ""key1"": ""string1"","& ASCII.LF &
         "   'key2'   : 'string2',"& ASCII.LF &
         "   'key3'   :'string3',"& ASCII.LF &
         "   'key4'   : 123,"& ASCII.LF &
         "   'key5'   : -123,"& ASCII.LF &
         "   'key6'   : 123.901239,"& ASCII.LF &
         "   'key7'   : -123.0,"& ASCII.LF &
         "   'key8'   : false,"& ASCII.LF &
         "   'key9'   : true,"& ASCII.LF &
         "   'key10'  : null"& ASCII.LF &
         "}   " & ASCII.LF & ASCII.HT);
      Parse (Request, T.P.all, Handler);
      Assert (Handler.I = Handler.Expected'Length,
              "parsing not correct"& Handler.I'Img &" /"&
              Handler.Expected'Length'Img);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Test_Flat;


   procedure Test_Nested (T : in out Test_Type)
   is
      use DB.Types.Values;
      Expected : constant Item_Array_Type :=
        ((Event => Anonymous_Object_Start),
         (Object_Start, TUS ("outer")),
         (Value, TUS ("key1"), New_Value ("string1")),
         (Value, TUS ("key2"), New_Value ("string2")),
         (Object_Start, TUS ("inner")),
         (Value, TUS ("key3"), New_Value ("string3")),
         (Value, TUS ("key4"), New_Value ("string4")),
         (Event => Object_End),
         (Event => Object_End),
         (Value, TUS ("key5"), New_Value ("string5")),
         (Event => Object_End));
      Request : AWS.Status.Data;
      Handler : Handler_Type := (Expected'Length, Expected, others => <>);
   begin
      Init_Body
        (Request,
         "{"&
         "   'outer': {,"& ASCII.LF &
         "      'key1':'string1',"& ASCII.LF &
         "      'key2':'string2',"& ASCII.LF &
         "      'inner': {,"& ASCII.LF &
         "         'key3':'string3',"& ASCII.LF &
         "         'key4':'string4',"& ASCII.LF &
         "       }," & ASCII.LF & ASCII.HT &
         "   }," & ASCII.LF & ASCII.HT &
         "   'key5':'string5',"& ASCII.LF &
         "}   " & ASCII.LF & ASCII.HT);
      Parse (Request, T.P.all, Handler);
      Assert (Handler.I = Handler.Expected'Length,
              "parsing not correct"& Handler.I'Img &" /"&
              Handler.Expected'Length'Img);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Test_Nested;


   procedure Test_Array (T : in out Test_Type)
   is
      use DB.Types.Values;
      Expected : constant Item_Array_Type :=
        ((Event => Anonymous_Object_Start),
         (Array_Start, TUS ("outer")),
         (Anonymous_Value, TUS(""), New_Value ("string1")),
         (Anonymous_Value, TUS(""), New_Value ("string2")),
         (Event => Anonymous_Array_Start),
         (Anonymous_Value, TUS(""), New_Value ("string3")),
         (Anonymous_Value, TUS(""), New_Value ("string4")),
         (Event => Array_End),
         (Event => Array_End),
         (Value, TUS ("huhu"), New_Value ("string5")),
         (Event => Object_End));
      Request : AWS.Status.Data;
      Handler : Handler_Type := (Expected'Length, Expected, others => <>);
   begin
      Init_Body
        (Request,
         "{"&
         "   'outer': [,"& ASCII.LF &
         "      'string1',"& ASCII.LF &
         "      'string2',"& ASCII.LF &
         "      ["& ASCII.LF &
         "         'string3',"& ASCII.LF &
         "         'string4',"& ASCII.LF &
         "      ]," & ASCII.LF & ASCII.HT &
         "   ]," & ASCII.LF & ASCII.HT &
         "   'huhu':'string5'"& ASCII.LF &
         "}   " & ASCII.LF & ASCII.HT);
      Parse (Request, T.P.all, Handler);
      Assert (Handler.I = Handler.Expected'Length,
              "parsing not correct"& Handler.I'Img &" /"&
              Handler.Expected'Length'Img);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Test_Array;


   procedure Test_Escape (T : in out Test_Type)
   is
      use DB.Types.Values;
      Expected : constant Item_Array_Type :=
        ((Event => Anonymous_Object_Start),
         (Value, TUS ("key1"), New_Value ("""string1""")),
         (Value, TUS ("key2"), New_Value ("""string2""")),
         (Value, TUS ("key3"), New_Value ("'string3'")),
         (Value, TUS ("key4"), New_Value ("'string4'")),
         (Value, TUS ("key5"), New_Value ("string5\")),
         (Value, TUS ("key6"), New_Value ("string6\""")),
         (Event => Object_End));
      Request : AWS.Status.Data;
      Handler : Handler_Type := (Expected'Length, Expected, others => <>);
   begin
      Init_Body
        (Request,
         "{"&
         "   ""key1"" : '""string1""',"& ASCII.LF &
         "   ""key2"" : ""\""string2\"""","& ASCII.LF &
         "   ""key3"" : '\'string3\'',"& ASCII.LF &
         "   'key4'   : ""'string4'"","& ASCII.LF &
         "   'key5'   :'string5\\',"& ASCII.LF &
         "   'key6'   :""string6\\\"""","& ASCII.LF &
         "}   " & ASCII.LF & ASCII.HT);
      Parse (Request, T.P.all, Handler);
      Assert (Handler.I = Handler.Expected'Length,
              "parsing not correct"& Handler.I'Img &" /"&
              Handler.Expected'Length'Img);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Test_Escape;

end REST.Input_Formats.JSON.Test;

