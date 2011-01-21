-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AUnit.Assertions; use AUnit.Assertions;

with AWS.Client;
with AWS.Headers;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.Net;
with AWS.Response;
with AWS.Server;

with REST.Handler;
with REST.Maps.Test_Utils;

package body REST.Put.Test is

   Base_Port : constant := 8080;
   Port      : Natural := 0;

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


   function KV (Row, Column, Value : String) return Key_Value_Type is
   begin
      return (TUS (Row), TUS (Column), TUS (Value));
   end KV;


   function URL (Table : String; Row : String := "") return String
   is
      function Img (N : Integer) return String
      is
         S : constant String := Integer'Image (N);
      begin
         if S (S'First) = ' ' then
            return S (S'First + 1 .. S'Last);
         else
            return S;
         end if;
      end Img;
   begin
      return "http://localhost:"& Img (Port) &"/"& Table &"/"& Row;
   end URL;


   function URL (Table : String; Row : Unbounded_String) return String is
   begin
      return URL (Table, TS (Row));
   end URL;


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


   procedure Start_Server (WS : in out AWS.Server.HTTP)
   is
      subtype Port_Range is Natural range 1 .. 10;
   begin
      Maps.Test_Utils.Delete_And_Create_Maps;
      for I in Port_Range loop
         declare
         begin
            Port := Base_Port + I;
            AWS.Server.Start
              (Web_Server => WS,
               Name       => "dingsbums-restful",
               Callback   => REST.Handler.Handler,
               Port       => Port);
            Put_Line ("Starting server at port "& Port'Img);
            exit;
         exception
            when AWS.Net.Socket_Error =>
               Put_Line ("Starting server at port "& Port'Img &" FAILED");
               if I = Port_Range'Last then
                  raise;
               end if;
         end;
      end loop;
   end Start_Server;


   function Put
     (URL  : String;
      Data : Unbounded_String)
      return AWS.Response.Data
   is
      Headers : AWS.Headers.List := AWS.Headers.Empty_List;
   begin
      AWS.Headers.Set.Add (Headers, "Content-Type", "application/json");
      return AWS.Client.Put (URL => URL, Data => TS (Data), Headers => Headers);
   end Put;


   procedure Put
     (Table      : in String;
      Key_Values : in Key_Value_Array_Type;
      Nested     : in Boolean)
   is
      S             : Unbounded_String;
      Last_Row      : Unbounded_String;
      Last_Row_Init : Boolean := False;
   begin
      if Nested then
         S := TUS ("{"& ASCII.LF);
         for I in Key_Values'Range loop
            if not Last_Row_Init or else Last_Row /= Key_Values (I).Row then
               if not Last_Row_Init then
                  Last_Row_Init := True;
               else
                  Append (S, "  },"& ASCII.LF);
               end if;
               Last_Row := Key_Values (I).Row;
               Append (S, "  '");
               Append (S, Key_Values (I).Row);
               Append (S, "' : {"& ASCII.LF);
            end if;
            Append (S, "    '");
            Append (S, Key_Values (I).Column);
            Append (S, "' : ");
            Append (S, Key_Values (I).Value);
            Append (S, ","& ASCII.LF);
         end loop;
         if Last_Row_Init then
            Append (S, "  }"& ASCII.LF);
         end if;
         Append (S, "}"& ASCII.LF);
         declare
            use type AWS.Messages.Status_Code;
            Response : constant AWS.Response.Data := Put (URL (Table), S);
         begin
            Assert (AWS.Response.Status_Code (Response) = AWS.Messages.S200,
                    "HTTP PUT of "& URL (Table) & ASCII.LF &
                    TS (S) &" not successful");
         end;

      else

         for I in Key_Values'Range loop
            if not Last_Row_Init or else Last_Row /= Key_Values (I).Row then
               if not Last_Row_Init then
                  Last_Row_Init := True;
               else
                  Append (S, "}");
                  declare
                     use type AWS.Messages.Status_Code;
                     Response : constant AWS.Response.Data :=
                        Put (URL (Table, Last_Row), S);
                  begin
                     Assert
                       (AWS.Response.Status_Code (Response) = AWS.Messages.S200,
                        "HTTP PUT of "& URL (Table, Last_Row) & ASCII.LF &
                        TS (S) &" not successful");
                  end;
               end if;
               Last_Row := Key_Values (I).Row;
               S := TUS ("{"& ASCII.LF);
            end if;
            Append (S, "  '");
            Append (S, Key_Values (I).Column);
            Append (S, "' : ");
            Append (S, Key_Values (I).Value);
            Append (S, ","& ASCII.LF);
         end loop;
         if Last_Row_Init then
            Append (S, "}");
         end if;
         declare
            use type AWS.Messages.Status_Code;
            Response : constant AWS.Response.Data :=
               Put (URL (Table, Last_Row), S);
         begin
            Assert
              (AWS.Response.Status_Code (Response) = AWS.Messages.S200,
               "HTTP PUT of "& URL (Table, Last_Row) & ASCII.LF &
               TS (S) &" not successful");
         end;
      end if;
   end Put;


   procedure Test_Flat (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Put ("test",
           (KV ("mensch1", "vorname", "'Sergey'"),
            KV ("mensch1", "nachname", "'Brin'"),
            KV ("mensch1", "alter", "37"),
            KV ("mensch2", "vorname", "'Larry'"),
            KV ("mensch2", "nachname", "'Page'"),
            KV ("mensch2", "alter", "37"),
            KV ("mensch3", "vorname", "'Julietta'"),
            KV ("mensch3", "nachname", "'Youtubehausenspetergoogle'"),
            KV ("mensch3", "alter", "25"),
            KV ("mensch4", "vorname", "'Eva'"),
            KV ("mensch4", "nachname", "'Gruenwaldenshausen'"),
            KV ("mensch4", "alter", "26"),
            KV ("mensch5", "vorname", "'Rudi'"),
            KV ("mensch5", "nachname", "'Fichtenwald'"),
            KV ("mensch5", "alter", "24")),
           Nested => False);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Test_Flat;


   procedure Test_Nested (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Put ("test",
           (KV ("person1", "vorname", "'Sergey'"),
            KV ("person1", "nachname", "'Brin'"),
            KV ("person1", "alter", "37"),
            KV ("person2", "vorname", "'Larry'"),
            KV ("person2", "nachname", "'Page'"),
            KV ("person2", "alter", "37"),
            KV ("person3", "vorname", "'Julietta'"),
            KV ("person3", "nachname", "'Youtubehausenspetergoogle'"),
            KV ("person3", "alter", "25"),
            KV ("person4", "vorname", "'Eva'"),
            KV ("person4", "nachname", "'Gruenwaldenshausen'"),
            KV ("person4", "alter", "26"),
            KV ("person5", "vorname", "'Rudi'"),
            KV ("person5", "nachname", "'Fichtenwald'"),
            KV ("person5", "alter", "24")),
           Nested => True);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Test_Nested;


   procedure Test_Flat_Empty (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Put ("test", (1 .. 0 => <>), Nested => False);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Test_Flat_Empty;


   procedure Test_Nested_Empty (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Put ("test", (1 .. 0 => <>), Nested => True);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Test_Nested_Empty;

end REST.Put.Test;

