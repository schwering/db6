with Ada.Text_IO;

with AWS.Messages;
with AWS.Server;
with AWS.Status;
with AWS.Response;

with DB.Maps;

with REST.Method.Delete;
with REST.Method.Get;
with REST.Method.Head;
with REST.Method.Options;
with REST.Method.Post;
with REST.Method.Put;

with REST.Error_Log;

procedure REST.Main is

   function Starts_With (S, T : String) return Boolean is
   begin
      return S'Length >= T'Length and then
             S (S'First .. S'First + T'Length - 1) = T;
   end Starts_With;


   function Handler (Request : AWS.Status.Data) return AWS.Response.Data is
      use AWS.Status;
   begin
      case AWS.Status.Method (Request) is
         when DELETE =>
            return Method.Delete (Request);
         when GET =>
            return Method.Get (Request);
         when HEAD =>
            return Method.Head (Request);
         when OPTIONS =>
            return Method.Options (Request);
         when POST =>
            return Method.Post (Request);
         when PUT =>
            return Method.Put (Request);
         when TRACE | CONNECT | EXTENSION_METHOD =>
            Error_Log.Push ("Invalid request");
            return AWS.Response.Acknowledge (AWS.Messages.S404,
                                             "Unsupported operation");
      end case;
   end Handler;

   WS : AWS.Server.HTTP;
begin
   Ada.Text_IO.Put_Line ("Hello");

   AWS.Server.Start
     (Web_Server     => WS,
      Name           => "dingsbums-restful",
      Callback       => Handler'Unrestricted_Access,
      Port           => 8080);

   declare
      C : Character;
   begin
      Ada.Text_IO.Get (C);
   end;
end REST.Main;

