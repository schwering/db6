-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with AWS.Client.HTTP_Utils;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;

function AWS.Client.Delete
  (URL        : String;
   User       : String          := No_Data;
   Pwd        : String          := No_Data;
   Proxy      : String          := No_Data;
   Proxy_User : String          := No_Data;
   Proxy_Pwd  : String          := No_Data;
   Timeouts   : Timeouts_Values := No_Timeout;
   Headers    : Header_List     := Empty_Header_List)
   return Response.Data
is

   use AWS.Client.HTTP_Utils;

   procedure Delete
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List)
   is
      use Ada.Real_Time;
      Stamp         : constant Time := Clock;
      Keep_Alive    : Boolean;
      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
   begin
      Retry : loop

         begin
            Open_Send_Common_Header (Connection, "DELETE", URI, Headers);

            Net.Buffered.New_Line (Connection.Socket.all);

            --  Get answer from server

            Parse_Header
              (Connection, Result, Keep_Alive);

            if not Keep_Alive then
               Disconnect (Connection);
            end if;

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception
            when E : Net.Socket_Error =>
               Debug_Exception (E);

               Disconnect (Connection);

               if Try_Count = 0
                 or else Clock - Stamp >= Connection.Timeouts.Response
               then
                  Result := Response.Build
                    (MIME.Text_HTML, "Delete Timeout", Messages.S408);
                  exit Retry;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop Retry;
   end Delete;

   Connection : HTTP_Connection;
   Result     : Response.Data;
begin
   Create (Connection,
           URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
           Persistent => False,
           Timeouts   => Timeouts);

   Delete (Connection, Result, Headers => Headers);
   Close (Connection);
   return Result;

exception
   when others =>
      Close (Connection);
      raise;
end AWS.Client.Delete;

