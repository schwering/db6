-- Abstract:
--
-- Handles GET HTTP requests.
--
-- Copyright 2010--2011 Christoph Schwering

with AWS.Status;
with AWS.Response;

function REST.Method.Get (Request : AWS.Status.Data) return AWS.Response.Data;

