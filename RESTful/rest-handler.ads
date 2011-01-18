-- Abstract:
--
-- The AWS handler for HTTP requests. It dispatches to the respective
-- REST.Method children.
--
-- Copyright 2008, 2009, 2010, 2011 Christoph Schwering

with AWS.Response;
with AWS.Status;

package REST.Handler is

   Handler : constant AWS.Response.Callback;

private
   function Handle (Request : AWS.Status.Data) return AWS.Response.Data;

   Handler : constant AWS.Response.Callback := Handle'Access;

end REST.Handler;

