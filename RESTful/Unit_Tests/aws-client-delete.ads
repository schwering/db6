-- Abstract:
--
-- Adds an client implementation of HTTP DELETE.
--
-- Copyright 2008--2011 Christoph Schwering

function AWS.Client.Delete
  (URL        : String;
   User       : String          := No_Data;
   Pwd        : String          := No_Data;
   Proxy      : String          := No_Data;
   Proxy_User : String          := No_Data;
   Proxy_Pwd  : String          := No_Data;
   Timeouts   : Timeouts_Values := No_Timeout;
   Headers    : Header_List     := Empty_Header_List)
   return Response.Data;

