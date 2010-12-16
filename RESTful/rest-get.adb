with Ada.Strings.Unbounded;

with AWS.Status;
with AWS.Response;
with AWS.URL;

with REST.Error_Log;
with REST.Maps;
with REST.Path_Parsers;

function REST.Get (Request : AWS.Status.Data) return AWS.Response.Data
is
   URL : constant AWS.URL.Object := AWS.Status.URI (Request);

   function Parse (P : String) return String is
      use Ada.Strings.Unbounded;
      use REST.Path_Parsers;
      S : Unbounded_String := To_Unbounded_String ("");
      I : Iterator_Type;
   begin
      Append (S, "<ul>");
      loop
         Next (P, I);
         exit when I = Final;
         Append (S, "<li> "& Value (P, I));
      end loop;
      Append (S, "<li> end");
      Append (S, "</ul>");
      return To_String (S);
   exception
      when E : others =>
         Error_Log.Push (E);
         return "error";
   end;

begin
   return AWS.Response.Build
     (Content_Type => "text/html",
      Message_Body => "<p>" & AWS.URL.Path (AWS.Status.URI (Request)) &
                      "<p>" & AWS.URL.Pathname (AWS.Status.URI (Request)) &
                      "<p>" & Parse (AWS.URL.Pathname (AWS.Status.URI (Request))));
end REST.Get;

