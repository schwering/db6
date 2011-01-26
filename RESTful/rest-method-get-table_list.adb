-- Abstract:
--
-- Responds with some debug information.
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Strings.Unbounded;

with AWS.Status;
with AWS.Response;
with AWS.URL;

with REST.Maps;

separate (REST.Method.Get)
procedure Table_List
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
   use Ada.Strings.Unbounded;
   URL        : constant AWS.URL.Object := AWS.Status.URI (Request);
   P          : constant String := AWS.URL.Pathname (URL);
   URL_Prefix : constant String := AWS.URL.Protocol_Name (URL) &"://"&
                                   AWS.URL.Host (URL) &":"&
                                   AWS.URL.Port (URL) &"/";
   URL_Suffix : constant String := "/*/"& Infinity_Row &
                                   "/"& Infinity_Row &
                                   "?"& Count_Param &"="& Img (Default_Count);
   S          : Unbounded_String;
begin
   if P /= "" and P /= "/" then
      Success := False;
   end if;

   Append (S, "{" & ASCII.LF);
   for I in 1 .. Maps.Count loop
      declare
         Name : constant String := Maps.Map_Name (I);
         URL  : constant String := URL_Prefix & Name & URL_Suffix;
      begin
         Append (S, "  """& Name &"""");
         Append (S, " : ");
         Append (S, """"& URL &"""");
         if I < Maps.Count then
            Append (S, ",");
         end if;
         Append (S, ""& ASCII.LF);
      end;
   end loop;
   Append (S, "}");

   Response := AWS.Response.Build
     (Status_Code  => AWS.Messages.S200,
      Content_Type => "text/plain",
      Message_Body => To_String (S));
   Success := True;
exception
   when E : others =>
      Log.Error (E);
      Success := False;
end;


