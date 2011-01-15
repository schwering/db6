-- Abstract:
--
-- Inserts a key/value pair into the map.
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Strings.Unbounded;

with AWS.Messages;
with AWS.Status;
with AWS.Response;
with AWS.URL;

with REST.Input_Formats;
with REST.Input_Formats.JSON;
with REST.Maps;
with REST.Path_Parsers;

separate (REST.Method.Put)
procedure Insert
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
   URL  : constant AWS.URL.Object := AWS.Status.URI (Request);
   Path : constant String := AWS.URL.Pathname (URL);
   Iter : Path_Parsers.Iterator_Type;

--   function Param (S : String) return String is
--   begin
--      return AWS.URL.Parameter (URL, S);
--   end Param;

   function Next_Path_Element return String is
   begin
      if Path_Parsers.Is_Final (Iter) then
         return "";
      end if;
      Path_Parsers.Next (Path, Iter);
      if Path_Parsers.Is_Final (Iter) then
         return "";
      end if;
      return Path_Parsers.Value (Path, Iter);
   end Next_Path_Element;

   Map_Name : constant String := Path_Parsers.Element (URL, 1);
   Row      : constant String := Next_Path_Element;
begin
   if Map_Name = "" then
      Success := False;
      return;
   end if;

   declare
      use type DB.Maps.State_Type;
      Map : constant Maps.Map_Ref_Type := Maps.Map_By_Name (Map_Name);
      --Key : constant DB.Maps.Keys.Key_Type := DB.Maps.Strings_To_Key (Row, "Col");
      --Val : constant DB.Maps.Value_Type'Class := To_Value ("Value");
      St  : DB.Maps.State_Type;
   begin
      --Map.Insert (Key, Val, St);
      --case St is
         --when DB.Maps.Success =>
            --Response := AWS.Response.Build
              --(Status_Code  => AWS.Messages.S200,
               --Content_Type => "text/plain",
               --Message_Body => "ok");
         --when DB.Maps.Failure =>
            --Response := AWS.Response.Build
              --(Status_Code  => AWS.Messages.S500,
               --Content_Type => "text/plain",
               --Message_Body => "error");
      --end case;
      Success := True;
   end;
end Insert;

