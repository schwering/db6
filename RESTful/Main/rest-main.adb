with Ada.Text_IO;

with AWS.Server;

with REST.Handler;

procedure REST.Main is
   WS : AWS.Server.HTTP;
begin
   Ada.Text_IO.Put_Line ("Hello");

   AWS.Server.Start
     (Web_Server => WS,
      Name       => "dingsbums-restful",
      Callback   => REST.Handler.Handler,
      Port       => 8080);

   declare
      C : Character;
   begin
      Ada.Text_IO.Get (C);
   end;
end REST.Main;

