-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with AWS.Status;
with AWS.Response;

function REST.Handler.Options (Request : AWS.Status.Data) return AWS.Response.Data
is
   pragma Unreferenced (Request);
begin
   return AWS.Response.Build
     (Content_Type => "text/html",
      Message_Body => "OPTIONS");
end REST.Handler.Options;

