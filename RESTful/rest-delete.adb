with AWS.Status;
with AWS.Response;

function REST.Delete (Request : AWS.Status.Data) return AWS.Response.Data
is
begin
   return AWS.Response.Build
     (Content_Type => "text/html",
      Message_Body => "DELETE");
end REST.Delete;

