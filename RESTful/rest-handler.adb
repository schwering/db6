-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with AWS.Messages;
with AWS.Status;

with REST.Method.Delete;
with REST.Method.Get;
with REST.Method.Head;
with REST.Method.Options;
with REST.Method.Post;
with REST.Method.Put;

with REST.Log;

package body REST.Handler is

   function Handle (Request : AWS.Status.Data) return AWS.Response.Data is
      use AWS.Status;
   begin
      case AWS.Status.Method (Request) is
         when DELETE =>  return Method.Delete (Request);
         when GET =>     return Method.Get (Request);
         when HEAD =>    return Method.Head (Request);
         when OPTIONS => return Method.Options (Request);
         when POST =>    return Method.Post (Request);
         when PUT =>     return Method.Put (Request);
         when TRACE | CONNECT | EXTENSION_METHOD =>
            Log.Error ("Invalid request");
            return AWS.Response.Acknowledge (AWS.Messages.S501,
                                             "Unsupported operation");
      end case;
   end Handle;

end REST.Handler;

