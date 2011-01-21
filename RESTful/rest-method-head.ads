-- Abstract:
--
-- Handles HEAD HTTP requests.
--
-- Copyright 2010--2011 Christoph Schwering

with AWS.Status;
with AWS.Response;

function REST.Method.Head (Request : AWS.Status.Data) return AWS.Response.Data;

