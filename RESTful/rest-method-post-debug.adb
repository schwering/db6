-- Abstract:
--
-- Responds with some debug information.
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Strings.Unbounded;

with AWS.Status;
with AWS.Response;
with AWS.URL;

with REST.Path_Parsers;

separate (REST.Method.Post)
procedure Debug
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
   use Ada.Strings.Unbounded;
   use REST.Path_Parsers;

   URL : constant AWS.URL.Object := AWS.Status.URI (Request);
   P   : constant String := AWS.URL.Pathname (URL);
   S   : Unbounded_String := To_Unbounded_String ("");
   I   : Iterator_Type;
begin
   loop
      Next (P, I);
      exit when Is_Final (I);
      Append (S, "   * "& Value (P, I));
   end loop;

   Response := AWS.Response.Build
     (Content_Type => "text/plain",
      Message_Body => AWS.URL.Path (AWS.Status.URI (Request)) & ASCII.LF &
                      AWS.URL.Pathname (AWS.Status.URI (Request)) & ASCII.LF &
                      To_String (S));
   Success := True;
exception
   when E : others =>
      Log.Error (E);
      Success := False;
end;

