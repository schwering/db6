-- Abstract:
--
-- Handles HTTP GET requests.
-- The GET handler dispatches to some internal handlers which are implemented as
-- separate procedures. The first handler which succeeds determines the
-- response. If no handler wants to handle the request, an error is logged and a
-- short error response is performed.
--
-- Copyright 2010--2011 Christoph Schwering

with AWS.Messages;
with AWS.Status;
with AWS.Response;

with REST.Log;

function REST.Handler.Get (Request : AWS.Status.Data) return AWS.Response.Data
is
   procedure Query
     (Request  : in  AWS.Status.Data;
      Response : out AWS.Response.Data;
      Success  : out Boolean);
   procedure Query
      (Request  : in  AWS.Status.Data;
       Response : out AWS.Response.Data;
       Success  : out Boolean)
   is separate;

   procedure Table_List
     (Request  : in  AWS.Status.Data;
      Response : out AWS.Response.Data;
      Success  : out Boolean);
   procedure Table_List
      (Request  : in  AWS.Status.Data;
       Response : out AWS.Response.Data;
       Success  : out Boolean)
   is separate;

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
     (Query'Access,
      Table_List'Access,
      Debug'Access);
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
      Message_Body => "No handler in REST.Handler.Get found");
end REST.Handler.Get;

