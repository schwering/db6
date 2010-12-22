with AWS.Status;
with AWS.Response;

function REST.Method.Head (Request : AWS.Status.Data) return AWS.Response.Data
is
begin
   return AWS.Response.Build
     (Content_Type => "text/html",
      Message_Body => "HEAD");
end REST.Method.Head;
