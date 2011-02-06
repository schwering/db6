-- Abstract:
--
-- Test utilities.
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Strings.Unbounded;

with AWS.Server;

package REST.Maps.Test_Utils is
   use Ada.Strings.Unbounded;

   Base_Port : constant := 8080;
   Port      : Natural := Base_Port;

   type Key_Value_Type is
      record
         Row    : Unbounded_String;
         Column : Unbounded_String;
         Value  : Unbounded_String;
      end record;

   type Key_Value_Array_Type is array (Positive range <>) of Key_Value_Type;


   function TS (S : Unbounded_String) return String
   renames To_String;

   function TUS (S : String) return Unbounded_String
   renames To_Unbounded_String;

   function KV (Row, Column, Value : String) return Key_Value_Type;

   function URL (Table : String; Row_1, Row_2 : String := "") return String;
   function URL (Table : String; Row : Unbounded_String) return String;
   function URL
     (Table, Col_Regexp, Row_1, Row_2 : String;
      Excl_1 : Boolean := False;
      Excl_2 : Boolean := False;
      Count  : Natural := 100)
      return String;

   procedure Start_Server (WS : in out AWS.Server.HTTP);

   procedure Unlink (File_Name : in String);
   procedure Delete_And_Create_Maps;

end REST.Maps.Test_Utils;

