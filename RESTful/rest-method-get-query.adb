-- Abstract:
--
-- Queries a map.
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Strings.Unbounded;

with AWS.Status;
with AWS.Response;
with AWS.URL;

with REST.Output_Formats;
with REST.Output_Formats.JSON;
with REST.Maps;
with REST.Path_Parsers;

separate (REST.Method.Get)
procedure Query
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
   URL   : constant AWS.URL.Object := AWS.Status.URI (Request);
   Path  : constant String := AWS.URL.Pathname (URL);
   Iter  : Path_Parsers.Iterator_Type;

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

   Map_Name  : constant String := Next_Path_Element;
   Key       : constant String := Next_Path_Element;
   Attribute : constant String := Next_Path_Element;
begin
   if Map_Name = "" or Key = "" or AWS.URL.Parameters (URL) /= "" or
      Map_Name'Length + Key'Length + Attribute'Length + 4 >= Path'Length
   then
      Success := False;
      return;
   end if;

   declare
      Map    : constant Maps.Map_Ref_Type := Maps.Map_By_Name (Map_Name);
      Cursor : constant DB.Maps.Cursor_Ref_Type := Map.New_Cursor_Ref
        (Thread_Safe => False,
         Lower_Bound => Lower_Bound,
         Upper_Bound => Upper_Bound);
      Stream : constant Output_Formats.Stream_Ref_Type :=
        new Output_Formats.JSON.Stream_Type;
   begin
      Output_Formats.Initialize_Stream
        (Stream, Cursor, Free_On_Close => True, Max_Objects => Count);
      Response := AWS.Response.Stream
        (Content_Type => Stream.Content_Type,
         Handle       => Stream,
         Server_Close => True);
      Success := True;
   end;
end Query;

