-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with AWS.Messages;
with AWS.Status;
with AWS.Response;

with REST.Log;

function REST.Handler.Post (Request : AWS.Status.Data) return AWS.Response.Data
is
   procedure Debug
     (Request  : in  AWS.Status.Data;
      Response : out AWS.Response.Data;
      Success  : out Boolean);
   procedure Debug
      (Request  : in  AWS.Status.Data;
       Response : out AWS.Response.Data;
       Success  : out Boolean)
   is separate;

   type Handler_type is access procedure
      (Request  : in  AWS.Status.Data;
       Response : out AWS.Response.Data;
       Success  : out Boolean);

   Handlers : constant array (Positive range <>) of Handler_Type :=
     (1 => Debug'Access);
begin
   for I in Handlers'Range loop
      declare
         Response : AWS.Response.Data;
         Success  : Boolean;
      begin
         Handlers (I) (Request, Response, Success);
         if Success then
            return Response;
         end if;
      end;
   end loop;

   Log.Error ("No handler found");
   return AWS.Response.Build
     (Status_Code  => AWS.Messages.S500,
      Content_Type => "text/plain",
      Message_Body => "No handler in REST.Handler.Post found");
end REST.Handler.Post;

