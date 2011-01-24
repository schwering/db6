with Ada.Text_IO;

with AWS.Net;
with AWS.Server;

with REST.Handler;

procedure REST.Main is
   Port : constant := 8080;
   WS   : AWS.Server.HTTP;
begin
   for P in Port .. Port + 100 loop
      declare
      begin
         Ada.Text_IO.Put_Line ("Starting HTTP server on port"&
                               Integer'Image (P));
         AWS.Server.Start
           (Web_Server => WS,
            Name       => "dingsbums-restful",
            Callback   => REST.Handler.Handler,
            Port       => P);
         Ada.Text_IO.Put_Line ("Started HTTP server on port"&
                               Integer'Image (P));
         exit;
      exception
         when AWS.Net.Socket_Error =>
            Ada.Text_IO.Put_Line ("Starting HTTP server on port"&
                                  Integer'Image (P) &" failed");
      end;
   end loop;

   declare
      C : Character;
   begin
      Ada.Text_IO.Get (C);
   end;
end REST.Main;

