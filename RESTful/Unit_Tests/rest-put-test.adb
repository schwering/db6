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
with AWS.Response;
with AWS.Server;

with DB.Maps;
with DB.Types.Keys;
with DB.Types.Values;

with REST.Maps.Test_Utils; use REST.Maps.Test_Utils;

package body REST.Put.Test is

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

      declare
         Map : constant REST.Maps.Map_Ref_Type := REST.Maps.Map_By_Name (Table);
      begin
         for I in Key_Values'Range loop
            declare
               use type DB.Maps.State_Type;
               Key : constant DB.Types.Keys.Key_Type :=
                  DB.Maps.Strings_To_Key (TS (Key_Values (I).Row),
                                          TS (Key_Values (I).Column));
               Value : DB.Types.Values.Value_Type;
               State : DB.Maps.State_Type;
            begin
               Map.Search (Key, Value, State);
               Assert (State = DB.Maps.Success, "No element with key "&
                       TS (Key_Values (I).Row) &" / "&
                       TS (Key_Values (I).Column));
            end;
         end loop;
      end;
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
            KV ("mensch5", "alter", "24"),
            KV ("person6", "gehalt", "1.234"),
            KV ("person6", "doof", "true"),
            KV ("person6", "klug", "false"),
            KV ("person6", "auto", "null")),
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
            KV ("person5", "alter", "24"),
            KV ("person6", "gehalt", "1.234"),
            KV ("person6", "doof", "true"),
            KV ("person6", "klug", "false"),
            KV ("person6", "auto", "null")),
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

