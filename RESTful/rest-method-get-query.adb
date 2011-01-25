-- Abstract:
--
-- Queries a map.
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Strings.Unbounded;

with AWS;
with AWS.Status;
with AWS.Response;
with AWS.URL;

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
   URL  : constant AWS.URL.Object := AWS.Status.URI (Request);
   Path : constant String := AWS.URL.Pathname (URL);
   Iter : Path_Parsers.Iterator_Type;

   function Param (S : String; Def : String := "") return String
   is
      T : constant String := AWS.URL.Parameter (URL, S);
   begin
      if T = "" then
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
   Inclusive_1 : constant Boolean := Param (From_Excl_Param) /= Yes_Value;
   Inclusive_2 : constant Boolean := Param (To_Excl_Param) /= Yes_Value;
   Offset      : constant Natural := Param (Offset_Param, 0);
   Count       : constant Natural := Param (Count_Param, Natural'Last);
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

      use REST.Maps.Cursors;

      Map    : constant Maps.Map_Ref_Type := Maps.Map_By_Name (Map_Name);
      Cursor : constant DB.Maps.Cursor_Ref_Type := Map.New_Cursor_Ref
        (Thread_Safe   => False,
         Lower_Bound   => Bound (Row_1, Inclusive_1, Lower => True),
         Upper_Bound   => Bound (Row_2, Inclusive_2, Lower => False),
         Column_Regexp => Col_Regexp);
      Writer : constant Output_Formats.Writer_Ref_Type :=
        new Output_Formats.JSON.Writer_Type;
   begin
      Output_Formats.Initialize_Writer
        (Writer, Cursor, Free_On_Close => True, Max_Objects => Count);
      Response := AWS.Response.Stream
        (Content_Type => Writer.Content_Type,
         Handle       => Writer,
         Encoding     => AWS.Status.Preferred_Coding (Request),
         Server_Close => True);
      Success := True;
   end;
end Query;

