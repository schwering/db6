-- Abstract:
--
-- Queries a map.
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Strings.Unbounded;

with AWS;
with AWS.Parameters;
with AWS.Parameters.Set;
with AWS.Status;
with AWS.Response;
with AWS.URL;
with AWS.URL.Set;

with DB.Maps;

with REST.Output_Formats;
with REST.Output_Formats.JSON;
with REST.Maps;
with REST.Maps.Cursors;
with REST.Path_Parsers;

separate (REST.Method.Get)
procedure Query
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
   use Ada.Strings.Unbounded;

   function Format_Path (S : String) return String
   is
      Last : Natural := S'Last;
   begin
      for I in reverse S'Range loop
         exit when S (I) /= '/' and S (I) /= '?';
         Last := I - 1;
      end loop;
      return S (S'First .. Last);
   end Format_Path;

   URL  : constant AWS.URL.Object := AWS.Status.URI (Request);
   Path : constant String := Format_Path (AWS.URL.Pathname (URL));
   Iter : Path_Parsers.Iterator_Type;

   function Param (S : String; Def : String := "") return String
   is
      T : constant String := AWS.URL.Parameter (URL, S);
   begin
      if T /= "" then
         return T;
      else
         return Def;
      end if;
   end Param;

   function Param (S : String; Def : Integer) return Integer is
   begin
      return Integer'Value (Param (S, "no-int"));
   exception
      when Constraint_Error =>
         return Def;
   end Param;

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

   Map_Name    : constant String := Next_Path_Element;
   Col_Regexp  : constant String := Next_Path_Element;
   Row_1       : constant String := Next_Path_Element;
   Row_2       : constant String := Next_Path_Element;
   Incl_1      : constant Boolean := Param (From_Excl_Param) /= Yes_Value;
   Incl_2      : constant Boolean := Param (To_Excl_Param) /= Yes_Value;
   Offset      : constant Natural := Param (Offset_Param, 0);
   Count       : constant Natural := Param (Count_Param, Default_Count);

   function Next_URL return Unbounded_String is
   begin
      if Count = Natural'Last then
         return To_Unbounded_String ("");
      else
         declare
            Next_URL   : AWS.URL.Object := URL;
            Parameters : AWS.Parameters.List := AWS.URL.Parameters (URL);
            Offset_Str : constant String := Img (Offset + Count);
         begin
            AWS.Parameters.Set.Update (Parameters, Offset_Param, Offset_Str);
            AWS.URL.Set.Parameters (Next_URL, Parameters);
            return To_Unbounded_String (AWS.URL.URL (Next_URL));
         end;
      end if;
   end Next_URL;

begin
   if Map_Name = "" or Row_1 = "" or Col_Regexp = "" then
      Success := False;
      return;
   end if;

   declare
      function Row_2 return String is
      begin
         if Query.Row_2 = "" then
            return Row_1;
         else
            return Query.Row_2;
         end if;
      end Row_2;

      use type DB.Maps.Map_Ref_Type;

      Map    : constant Maps.Map_Ref_Type := Maps.Map_By_Name (Map_Name);
      Writer : Output_Formats.Writer_Ref_Type;
   begin
      if Map = null then
         Success := False;
         return;
      end if;
      Writer := new Output_Formats.JSON.Writer_Type;
      Output_Formats.Initialize_Writer
        (Writer            => Writer,
         Map               => Map,
         URL_Path          => To_Unbounded_String (Path),
         Offset            => Offset,
         Lower_Bound       => Maps.Cursors.Bound (Row_1, Incl_1, Lower => True),
         Upper_Bound       => Maps.Cursors.Bound (Row_2, Incl_2, Lower => False),
         Has_Column_Regexp => Col_Regexp /= Everything_Regexp,
         Column_Regexp     => Col_Regexp,
         Max_Objects       => Count,
         Next_URL          => Next_URL);
      Response := AWS.Response.Stream
        (Content_Type => Writer.Content_Type,
         Handle       => Writer,
         Encoding     => AWS.Status.Preferred_Coding (Request),
         Server_Close => True);
      Success := True;
   end;
end Query;

