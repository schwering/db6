-- Abstract:
--
-- Deletes an object a map.
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Strings.Unbounded;

with AWS;
with AWS.Status;
with AWS.Response;
with AWS.URL;

with DB.Maps;

with REST.Maps;
with REST.Path_Parsers;

separate (REST.Handler.Delete)
procedure Delete
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
   use Ada.Strings.Unbounded;

   URL  : constant AWS.URL.Object := AWS.Status.URI (Request);
   Path : constant String := AWS.URL.Pathname (URL);
   Iter : Path_Parsers.Iterator_Type;

   function Next_Path_Element return String is
   begin
      if Path_Parsers.Is_Final (Iter) then
         return "";
      end if;
      Path_Parsers.Next (Path, Iter);
      if Path_Parsers.Is_Final (Iter) then
         return "";
      end if;
      return Path_Parsers.Value (Path, Iter);
   end Next_Path_Element;

   Map_Name : constant String := Next_Path_Element;
   Row_1    : constant String := Next_Path_Element;
   Row_2    : constant String := Next_Path_Element;
begin
   if Map_Name = "" or Row_1 = "" then
      Success := False;
      return;
   end if;

   declare
      Row_2_Param : String renames Row_2;

      function Row_2 return String is
      begin
         if Row_2_Param = "" then
            return Row_1;
         else
            return Row_2_Param;
         end if;
      end Row_2;

      use type DB.Maps.Map_Ref_Type;
      use type DB.Maps.State_Type;

      Map   : constant Maps.Map_Ref_Type := Maps.Map_By_Name (Map_Name);
      First : constant DB.Types.Keys.Key_Type := Make_Key (Row_1, Max => False);
      Last  : constant DB.Types.Keys.Key_Type := Make_Key (Row_1, Max => True);
      State : DB.Maps.State_Type;
   begin
      if Map = null then
         Success := False;
         return;
      end if;
      Map.Delete_Range (First, Last, State);
      if State /= DB.Maps.Success then
         raise Deletion_Error;
      end if;
      Response := AWS.Response.Build
        (Status_Code  => AWS.Messages.S200,
         Content_Type => "text/plain",
         Message_Body => "ok");
      Success := True;
   exception
      when others =>
         Response := AWS.Response.Build
           (Status_Code  => AWS.Messages.S500,
            Content_Type => "text/plain",
            Message_Body => "error");
         raise;
   end;
end Delete;

