-- Abstract:
--
-- Handles DELETE HTTP requests.
--
-- Copyright 2010--2011 Christoph Schwering

with AWS.Status;
with AWS.Response;

function REST.Handler.Delete
  (Request : AWS.Status.Data)
   return AWS.Response.Data;

