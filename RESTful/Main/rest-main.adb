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
         if SSL then
            AWS.Server.Set_Security (WS, Certificate_Filename);
         end if;
         AWS.Server.Start
           (Web_Server => WS,
            Name       => "dingsbums-restful",
            Callback   => REST.Handler.Handler,
            Port       => P,
            Security   => SSL);
         Ada.Text_IO.Put_Line ("Started HTTP server on port"&
                               Integer'Image (P));
         exit;
      exception
         when AWS.Net.Socket_Error =>
            Ada.Text_IO.Put_Line ("Starting HTTP server on port"&
                                  Integer'Image (P) &" failed");
      end;
   end loop;
   AWS.Server.Wait (AWS.Server.Q_Key_Pressed);
end REST.Main;

