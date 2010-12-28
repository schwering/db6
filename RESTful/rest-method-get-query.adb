-- Abstract:
--
-- Queries a map.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Strings.Unbounded;

with AWS.Status;
with AWS.Response;
with AWS.URL;

with REST.Output_Formats;
with REST.Output_Formats.JSON;
with REST.Maps;
with REST.Path_Parsers;

separate (REST.Method.Get)
procedure Query
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
   function Count_String_To_Positive (S : String) return Positive is
   begin
      if S = "" then
         return 1;
      elsif S = "img" then
         return Positive'Last;
      else
         return Positive'Value (S);
      end if;
   exception
      when others =>
         return 1;
   end Count_String_To_Positive;

   URL       : constant AWS.URL.Object := AWS.Status.URI (Request);
   Map_Name  : constant String := Path_Parsers.Element (URL, 1);
   From      : constant String := Path_Parsers.Element (URL, 2);
   To        : constant String := Path_Parsers.Element (URL, 3);
   Count_S   : constant String := AWS.URL.Parameter (URL, Count_Param);
   Count     : constant Positive := Count_String_To_Positive (Count_S);
   Rev       : constant Boolean := AWS.URL.Parameter (URL, Reverse_Param) = "1";
   From_Incl : constant Boolean := AWS.URL.Parameter (URL, From_Excl_Param) /= "1";
   To_Incl   : constant Boolean := AWS.URL.Parameter (URL, To_Excl_Param) /= "1";
begin
   if Map_Name = "" then
      Success := False;
      return;
   end if;

   declare
      From_Key : constant DB.Maps.Key_Type := Make_Key (From, Max => Rev);
      To_Key   : constant DB.Maps.Key_Type := Make_Key (To, Max => not Rev);

      function Lower_Bound return DB.Maps.Bound_Type is
      begin
         if From = "" then
            return DB.Maps.Negative_Infinity_Bound;
         else
            case From_Incl is
               when False =>
                  return DB.Maps.New_Bound (DB.Maps.Greater, From_Key);
               when True =>
                  return DB.Maps.New_Bound (DB.Maps.Greater_Or_Equal, From_Key);
            end case;
         end if;
      end Lower_Bound;

      function Upper_Bound return DB.Maps.Bound_Type is
      begin
         if To = "" then
            return DB.Maps.Positive_Infinity_Bound;
         else
            case To_Incl is
               when False =>
                  return DB.Maps.New_Bound (DB.Maps.Less, To_Key);
               when True =>
                  return DB.Maps.New_Bound (DB.Maps.Less_Or_Equal, To_Key);
            end case;
         end if;
      end Upper_Bound;

      Map    : constant Maps.Map_Ref_Type := Maps.Map_By_Name (Map_Name);
      Cursor : constant DB.Maps.Cursor_Ref_Type := Map.New_Cursor_Ref
        (Thread_Safe => False,
         Lower_Bound => Lower_Bound,
         Upper_Bound => Upper_Bound);
      Stream : constant Output_Formats.Stream_Ref_Type :=
        new Output_Formats.JSON.Stream_Type;
   begin
      Output_Formats.Initialize_Stream
        (Stream, Cursor, Free_On_Close => True, Max_Objects => Count);
      Response := AWS.Response.Stream
        (Content_Type => Stream.Content_Type,
         Handle       => Stream,
         Server_Close => True);
      Success := True;
   end;
end Query;

