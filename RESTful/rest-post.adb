with AWS.Status;
with AWS.Response;

function REST.Post (Request : AWS.Status.Data) return AWS.Response.Data
is
begin
   return AWS.Response.Build
     (Content_Type => "text/html",
      Message_Body => "POST");
end REST.Post;

