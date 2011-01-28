-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with AWS.Messages;
with AWS.Response;

with REST.Maps;
with REST.Handler.Delete;
with REST.Handler.Get;
with REST.Handler.Head;
with REST.Handler.Options;
with REST.Handler.Post;
with REST.Handler.Put;

with REST.Log;

package body REST.Handler is

   function Handle (Request : AWS.Status.Data) return AWS.Response.Data is
      use AWS.Status;
      User     : constant String := Authorization_Name (Request);
      Password : constant String := Authorization_Password (Request);
   begin
      --if Maps.Is_Valid_User (User, Password) then
         case AWS.Status.Method (Request) is
            when DELETE  => return Delete (Request);
            when GET     => return Get (Request);
            when HEAD    => return Head (Request);
            when OPTIONS => return Options (Request);
            when POST    => return Post (Request);
            when PUT     => return Put (Request);
            when TRACE | CONNECT | EXTENSION_METHOD =>
               Log.Error ("Invalid request");
               return AWS.Response.Acknowledge (AWS.Messages.S501,
                                                "Unsupported operation");
         end case;
      --else
         --return AWS.Response.Authenticate ("AWS", AWS.Response.Basic);
      --end if;
   end Handle;

end REST.Handler;

