with Ada.Text_IO;

with AWS.Server;
with AWS.Status;
with AWS.Response;

with DB.Maps;

with REST.Maps;
with REST.Error_Log;

procedure REST.Main is

   function Starts_With (S, T : String) return Boolean is
   begin
      return S'Length >= T'Length and then
             S (S'First .. S'First + T'Length - 1) = T;
   end Starts_With;

   function Handler (Request : AWS.Status.Data) return AWS.Response.Data
   is
      URI  : constant String := AWS.Status.URI (Request);
      User : constant String := AWS.Status.Authorization_Name (Request);
   begin
      if Starts_With (URI, "/private") then
         if User /= "" then
            return AWS.Response.Build
              (Content_Type => "text/html",
               Message_Body => "<h1>RESTful Dingsbums</h1>" &
                               "<p>Hallo "& User &", du alter L&uuml;mmel, du " &
                               "bist auf "& URI &"!</p>");
         else
            return AWS.Response.Authenticate
              (Realm => "RESTful Dingsbums",
               Mode  => AWS.Response.Basic);
         end if;
      else
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body => "<h1>RESTful Dingsbums</h1>" &
                            "<p>Hallo du alter L&uuml;mmel, du " &
                            "bist auf "& URI &"!</p>");
      end if;
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

