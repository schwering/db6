-- Abstract:
--
-- Responds with some debug information.
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Strings.Unbounded;
with Ada.Text_IO;
use Ada.Text_IO;

with AWS.Status;
with AWS.Response;
with AWS.URL;

with REST.Maps;

separate (REST.Method.Get)
procedure Table_List
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
   use Ada.Strings.Unbounded;
   URL : constant AWS.URL.Object := AWS.Status.URI (Request);
   P   : constant String := AWS.URL.Pathname (URL);
   S   : Unbounded_String;
begin
   Put_Line ("Huhu");
   if P /= "" and P /= "/" then
      Success := False;
   end if;

   Append (S, "[" & ASCII.LF);
   for I in 1 .. Maps.Count loop
      Append (S, "  """& Maps.Map_Name (I) &"""");
      if I < Maps.Count then
         Append (S, ",");
      end if;
      Append (S, ""& ASCII.LF);
   end loop;
   Append (S, "]");

   Response := AWS.Response.Build
     (Status_Code  => AWS.Messages.S200,
      Content_Type => "text/plain",
      Message_Body => To_String (S));
   Success := True;
exception
   when E : others =>
      Log.Error (E);
      Success := False;
end;


