-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with AUnit.Assertions; use AUnit.Assertions;

with AWS.Client;
with AWS.Client.Delete;
with AWS.Messages;
with AWS.Response;
with AWS.Server;

with DB.Maps;
with DB.Types.Keys;
with DB.Types.Values;

with REST.Maps.Test_Utils; use REST.Maps.Test_Utils;

package body REST.Delete.Test is

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


   type Entry_Type is
      record
         Key   : DB.Types.Keys.Key_Type;
         Value : DB.Types.Values.Value_Type;
      end record;

   Table : constant String := "test";

   Entries : constant array (Positive range <>) of Entry_Type :=
     ((Make_Key ("mensch1","vorname",0), DB.Types.Values.New_Value ("Sergey")),
      (Make_Key ("mensch1","nachname",0), DB.Types.Values.New_Value ("Brin")),
      (Make_Key ("mensch1","alter",0), DB.Types.Values.New_Value ("37")),
      (Make_Key ("mensch2","vorname",0), DB.Types.Values.New_Value ("Larry")),
      (Make_Key ("mensch2","nachname",0), DB.Types.Values.New_Value ("Page")),
      (Make_Key ("mensch2","alter",0), DB.Types.Values.New_Value ("37")),
      (Make_Key ("mensch3","vorname",0), DB.Types.Values.New_Value ("Eric")),
      (Make_Key ("mensch3","nachname",0), DB.Types.Values.New_Value ("Schmidt")),
      (Make_Key ("mensch3","alter",0), DB.Types.Values.New_Value ("55")),
      (Make_Key ("mensch4","vorname",0), DB.Types.Values.New_Value ("Oma")),
      (Make_Key ("mensch4","nachname",0), DB.Types.Values.New_Value ("Schmitz")),
      (Make_Key ("mensch5","vorname",0), DB.Types.Values.New_Value ("Opa")),
      (Make_Key ("mensch5","nachname",0), DB.Types.Values.New_Value ("Schmitz")));

   subtype Row_Type is String (1..7);

   Rows : constant array (Positive range <>) of Row_Type :=
     ("mensch1", "mensch2", "mensch3", "mensch4", "mensch5", "mensch6");


   procedure Insertions (Table : in String)
   is
      use type DB.Maps.State_Type;
      Map   : constant REST.Maps.Map_Ref_Type := REST.Maps.Map_By_Name (Table);
      State : DB.Maps.State_Type;
   begin
      for I in Entries'Range loop
         Map.Insert (Entries (I).Key, Entries (I).Value, State);
         Assert (State = DB.Maps.Success, "insertion"& I'Img &" failed");
      end loop;
   end Insertions;


   procedure Delete
     (Table          : in String;
      First          : in String;
      Last_Or_Single : in String := "";
      Expect_Success : in Boolean := True)
   is
      use type AWS.Messages.Status_Code;

      function Last return String is
      begin
         if Last_Or_Single = "" then
            return First;
         else
            return Last_Or_Single;
         end if;
      end Last;

      Response : constant AWS.Response.Data := AWS.Client.Delete
        (URL => URL (Table, First, Last));
   begin
      Assert (AWS.Response.Status_Code (Response) = AWS.Messages.S200,
              "HTTP DELETE of "& URL (Table, First, Last) &" successful /= "&
              Expect_Success'Img);

      declare
         Response : constant AWS.Response.Data := AWS.Client.Delete
           (URL => URL (Table, Everything_Regexp, First, Last));
         Content  : constant String := AWS.Response.Message_Body (Response);
         Empty    : Boolean := True;
      begin
         for I in Content'Range loop
            if Content (I) /= ' ' and
               Content (I) /= ASCII.HT and 
               Content (I) /= ASCII.CR and 
               Content (I) /= ASCII.LF and 
               Content (I) /= '{' and
               Content (I) /= '}'
            then
               Empty := False;
            end if;
         end loop;
         Assert (Empty, "Response body not empty: "&
                 "'"& First &"' .. '"& Last &"':"& ASCII.LF & Content);
      end;
   end Delete;


   procedure Test_Single (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Insertions (Table);
      Delete (Table, "NONEXISTEND", Expect_Success => False);
      for I in Rows'Range loop
         Delete (Table, Rows (I));
      end loop;
      Delete (Table, "NONEXISTEND", Expect_Success => False);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Test_Single;


   procedure Test_Range (T : in out Test_Type)
   is
      pragma Unreferenced (T);
      WS : AWS.Server.HTTP;
   begin
      Start_Server (WS);
      Insertions (Table);
      Delete (Table, "NONEXISTEND1", "NONEXISTEND2", Expect_Success => False);
      for I in Rows'Range loop
         Delete (Table, Rows (I), Rows (Positive'Min (Rows'Last, I+2)));
      end loop;
      Delete (Table, "NONEXISTEND1", "NONEXISTEND2", Expect_Success => False);
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         raise;
   end Test_Range;

end REST.Delete.Test;

