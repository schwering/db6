-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

with Interfaces.C;

with AWS.Net;

with REST.Method;
with REST.Handler;
with REST.Log;

package body REST.Maps.Test_Utils is

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


   function KV (Row, Column, Value : String) return Key_Value_Type is
   begin
      return (TUS (Row), TUS (Column), TUS (Value));
   end KV;


   function URL (Table : String; Row : String := "") return String is
   begin
      return "http://localhost:"& Img (Port) &"/"& Table &"/"& Row;
   end URL;


   function URL (Table : String; Row : Unbounded_String) return String is
   begin
      return URL (Table, TS (Row));
   end URL;


   function URL
     (Table, Col_Regexp, Row_1, Row_2 : String;
      Excl_1 : Boolean := True;
      Excl_2 : Boolean := True;
      Count  : Natural := 100)
      return String
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

      function Excl_String (Param : String; Excl : Boolean) return String is
      begin
        if Excl then
          return Param &"="& REST.Method.Yes_Value &"&";
        else
           return "";
        end if;
      end Excl_String;

   begin
      return "http://localhost:"& Img (Port) &"/"& Table &"/"& Col_Regexp &
             "/"& Row_1 &"/"& Row_2 &"/?"&
             Excl_String (REST.Method.From_Excl_Param, Excl_1) &
             Excl_String (REST.Method.To_Excl_Param, Excl_2) &
             REST.Method.Count_Param &"="& Img (Count);
   end URL;


   procedure Start_Server (WS : in out AWS.Server.HTTP)
   is
      subtype Port_Range is Natural range 1 .. 100;
   begin
      Delete_And_Create_Maps;
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


   procedure Unlink (File_Name : in String)
   is
      use Interfaces.C;

      procedure Unlink (Pathname : char_array);
      pragma Import (C, Unlink, "unlink");

      C_File_Name : constant char_array := To_C (File_Name);
   begin
      Unlink (C_File_Name);
   end Unlink;


   function TS (S : Paths.Bounded_String) return String is
   begin
      return Paths.To_String (S);
   end TS;


   procedure Delete_And_Create_Maps is
   begin
      Initialized := True;

      for I in Infos'Range loop
         declare
            procedure Free is new Ada.Unchecked_Deallocation
              (DB.Maps.Map_Type'Class, Map_Ref_Type);
            Map : Map_Ref_Type;
         begin
            Unlink (TS (Infos (I).Path));
            if Maps.Contains (Infos (I).Name) then
               Map := Maps.Element (Infos (I).Name);
               Free (Map);
               Maps.Exclude (Infos (I).Name);
            end if;
            Map := DB.Maps.New_Map_Ref (Infos (I).Impl);
            DB.Maps.Create (Map.all, TS (Infos (I).Path));
            Maps.Insert (Infos (I).Name, Map);
         exception
            when E : others =>
               Log.Error (E);
         end;
      end loop;
   end Delete_And_Create_Maps;

end REST.Maps.Test_Utils;

