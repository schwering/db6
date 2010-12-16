with AWS.Status;
with AWS.Response;

function REST.Options (Request : AWS.Status.Data) return AWS.Response.Data
is
begin
   return AWS.Response.Build
     (Content_Type => "text/html",
      Message_Body => "OPTIONS");
end REST.Options;

