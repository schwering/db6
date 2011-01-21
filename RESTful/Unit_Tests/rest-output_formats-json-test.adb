-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Tags;

with AWS.Status;
with AWS.Status.Set;

with AUnit.Assertions; use AUnit.Assertions;

with DB.Maps.Values;
with DB.Maps.Values.Booleans;
with DB.Maps.Values.Long_Floats;
with DB.Maps.Values.Long_Integers;
with DB.Maps.Values.Nothings;
with DB.Maps.Values.Strings;

with REST.Input_Formats.JSON;

package body REST.Output_Formats.JSON.Test is

   overriding
   procedure Set_Up (T : in out Test_Type) is
   begin
      null;
   end Set_Up;


   overriding
   procedure Tear_Down (T : in out Test_Type) is
   begin
      null;
   end Tear_Down;


   procedure Init_Body
     (Request : in out AWS.Status.Data;
      Writer  : in out Writer_Type)
   is
      package AS renames Ada.Streams;

      Buffer : AS.Stream_Element_Array (1 .. 4096);
      Last   : AS.Stream_Element_Offset;
   begin
      loop
         Writer.Read (Buffer, Last);
         exit when Last not in Buffer'Range;
         AWS.Status.Set.Append_Body (Request, Buffer (1 .. Last));
      end loop;
   end Init_Body;


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
      Val     : in     DB.Maps.Value_Type'Class)
   is
      use type Ada.Tags.Tag;
      use type DB.Maps.Value_Type;
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Anonymous_Value,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got value");
      Assert (Handler.Expected (Handler.I).Value.Ref.Image = Val.Image and
              Handler.Expected (Handler.I).Value.Ref.all'Tag = Val'Tag,
              "expected '"& Handler.Expected (Handler.I).Value.Ref.Image &"', "&
              "got '"& Val.Image &"'");
   end Anonymous_Value;


   procedure Value
     (Handler : in out Handler_Type;
      Key     : in     String;
      Val     : in     DB.Maps.Value_Type'Class)
   is
      use type Ada.Tags.Tag;
      use type DB.Maps.Value_Type;
   begin
      Handler.I := Handler.I + 1;
      Assert (Handler.Expected (Handler.I).Event = Value,
              "expected "& Handler.Expected (Handler.I).Event'Img &", "&
              "got value");
      Assert (To_String (Handler.Expected (Handler.I).Key) = Key,
              "expected '"& To_String (Handler.Expected (Handler.I).Key) &"', "&
              "got '"& Key &"'");
      Assert (Handler.Expected (Handler.I).Value.Ref.Image = Val.Image and
              Handler.Expected (Handler.I).Value.Ref.all'Tag = Val'Tag,
              "expected '"& Handler.Expected (Handler.I).Value.Ref.Image &"', "&
              "got '"& Val.Image &"'");
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


   function TS (U : Unbounded_String) return String
   renames To_String;


   function TKW (V : DB.Maps.Value_Type'Class) return DB.Maps.Value_Wrapper_Type
   renames DB.Maps.New_Value_Wrapper;


   type Writer_Ref_Type is access all Writer_Type'Class;

   task type Writer_Task_Type is
      entry Start (Doc_Ptr    : in Item_Array_Ref_Type;
                   Writer_Ptr : in Writer_Ref_Type);
   end Writer_Task_Type;

   task body Writer_Task_Type
   is
      Doc    : Item_Array_Ref_Type;
      Writer : Writer_Ref_Type;
   begin
      accept Start (Doc_Ptr    : in Item_Array_Ref_Type;
                    Writer_Ptr : in Writer_Ref_Type) do
         Doc    := Doc_Ptr;
         Writer := Writer_Ptr;
      end Start;
      for I in Doc'Range loop
         case Doc (I).Event is
            when Anonymous_Object_Start =>
               Writer.Start_Anonymous_Object;
            when Object_Start =>
               Writer.Start_Object (TS (Doc (I).Key));
            when Object_End =>
               Writer.End_Object;
            when Anonymous_Array_Start =>
               Writer.Start_Anonymous_Array;
            when Array_Start =>
               Writer.Start_Array (TS (Doc (I).Key));
            when Array_End =>
               Writer.End_Array;
            when Value =>
               Writer.Put_Value (TS (Doc (I).Key), Doc (I).Value.Ref.all);
            when Anonymous_Value =>
               Writer.Put_Anonymous_Value (Doc (I).Value.Ref.all);
            when Error =>
               Assert (False, "There's an ERROR in Doc, stupid test");
         end case;
      end loop;
      Writer.Close;
   end Writer_Task_Type;


   procedure Test_Flat (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      use DB.Maps.Values;

      Doc : aliased Item_Array_Type :=
        ((Event => Anonymous_Object_Start),
         (Value, TUS ("key1"), TKW (Strings.New_Value ("string1"))),
         (Value, TUS ("key2"), TKW (Strings.New_Value ("string2"))),
         (Value, TUS ("key3"), TKW (Strings.New_Value ("string3"))),
         (Value, TUS ("key4"), TKW (Long_Integers.New_Value (123))),
         (Value, TUS ("key5"), TKW (Long_Integers.New_Value (-123))),
         (Value, TUS ("key6"), TKW (Long_Floats.New_Value (123.901239))),
         (Value, TUS ("key7"), TKW (Long_Floats.New_Value (-123.0))),
         (Value, TUS ("key8"), TKW (Booleans.New_Value (False))),
         (Value, TUS ("key9"), TKW (Booleans.New_Value (True))),
         (Value, TUS ("key10"), TKW (Nothings.New_Value)),
         (Event => Object_End));

      Writer      : aliased JSON.Writer_Type;
      Writer_Task : Writer_Task_Type;
      Request     : AWS.Status.Data;
      Parser      : Input_Formats.JSON.Parser_Type :=
         REST.Input_Formats.JSON.New_Parser;
      Handler     : Handler_Type :=
         (Expected => Doc'Unrestricted_Access, others => <>);
   begin
      Writer_Task.Start (Doc'Unrestricted_Access, Writer'Unrestricted_Access);
      Init_Body (Request, Writer);
      Input_Formats.Parse (Request, Parser, Handler);
   end Test_Flat;


   procedure Test_Nested (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      use DB.Maps.Values;

      Doc : aliased Item_Array_Type :=
        ((Event => Anonymous_Object_Start),
         (Object_Start, TUS ("outer")),
         (Value, TUS ("key1"), TKW (Strings.New_Value ("string1"))),
         (Value, TUS ("key2"), TKW (Strings.New_Value ("string2"))),
         (Object_Start, TUS ("inner")),
         (Value, TUS ("key3"), TKW (Strings.New_Value ("string3"))),
         (Value, TUS ("key4"), TKW (Strings.New_Value ("string4"))),
         (Event => Object_End),
         (Event => Object_End),
         (Value, TUS ("key5"), TKW (Strings.New_Value ("string5"))),
         (Event => Object_End));

      Writer      : aliased JSON.Writer_Type;
      Writer_Task : Writer_Task_Type;
      Request     : AWS.Status.Data;
      Parser      : Input_Formats.JSON.Parser_Type :=
         REST.Input_Formats.JSON.New_Parser;
      Handler     : Handler_Type :=
         (Expected => Doc'Unrestricted_Access, others => <>);
   begin
      Writer_Task.Start (Doc'Unrestricted_Access, Writer'Unrestricted_Access);
      Init_Body (Request, Writer);
      Input_Formats.Parse (Request, Parser, Handler);
   end Test_Nested;


   procedure Test_Array (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      use DB.Maps.Values;

      Doc : aliased Item_Array_Type :=
        ((Event => Anonymous_Object_Start),
         (Array_Start, TUS ("outer")),
         (Anonymous_Value, TUS(""), TKW (Strings.New_Value ("string1"))),
         (Anonymous_Value, TUS(""), TKW (Strings.New_Value ("string2"))),
         (Event => Anonymous_Array_Start),
         (Anonymous_Value, TUS(""), TKW (Strings.New_Value ("string3"))),
         (Anonymous_Value, TUS(""), TKW (Strings.New_Value ("string4"))),
         (Event => Array_End),
         (Event => Array_End),
         (Value, TUS ("huhu"), TKW (Strings.New_Value ("string5"))),
         (Event => Object_End));

      Writer      : aliased JSON.Writer_Type;
      Writer_Task : Writer_Task_Type;
      Request     : AWS.Status.Data;
      Parser      : Input_Formats.JSON.Parser_Type :=
         REST.Input_Formats.JSON.New_Parser;
      Handler     : Handler_Type :=
         (Expected => Doc'Unrestricted_Access, others => <>);
   begin
      Writer_Task.Start (Doc'Unrestricted_Access, Writer'Unrestricted_Access);
      Init_Body (Request, Writer);
      Input_Formats.Parse (Request, Parser, Handler);
   end Test_Array;


   procedure Test_Escaped (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      use DB.Maps.Values;

      Doc : aliased Item_Array_Type :=
        ((Event => Anonymous_Object_Start),
         (Value, TUS ("key1"), TKW (Strings.New_Value ("""string1"""))),
         (Value, TUS ("key2"), TKW (Strings.New_Value ("""string2"""))),
         (Value, TUS ("key3"), TKW (Strings.New_Value ("'string3'"))),
         (Value, TUS ("key4"), TKW (Strings.New_Value ("'string4'"))),
         (Value, TUS ("key5"), TKW (Strings.New_Value ("string5\"))),
         (Value, TUS ("key6"), TKW (Strings.New_Value ("string6\"""))),
         (Event => Object_End));

      Writer      : aliased JSON.Writer_Type;
      Writer_Task : Writer_Task_Type;
      Request     : AWS.Status.Data;
      Parser      : Input_Formats.JSON.Parser_Type :=
         REST.Input_Formats.JSON.New_Parser;
      Handler     : Handler_Type :=
         (Expected => Doc'Unrestricted_Access, others => <>);
   begin
      Writer_Task.Start (Doc'Unrestricted_Access, Writer'Unrestricted_Access);
      Init_Body (Request, Writer);
      Input_Formats.Parse (Request, Parser, Handler);
   end Test_Escaped;


end REST.Output_Formats.JSON.Test;

