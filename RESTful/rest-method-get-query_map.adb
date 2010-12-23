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
--with REST.Output_Formats.BSON;
with REST.Maps;
with REST.Path_Parsers;

separate (REST.Method.Get)
procedure Query_Map
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
   function Count_String_To_Positive (S : String) return Positive is
   begin
      if S = "" then
         return 1;
      else
         return Positive'Value (S);
      end if;
   exception
      when others =>
         return 1;
   end Count_String_To_Positive;

   URL      : constant AWS.URL.Object := AWS.Status.URI (Request);
   Map_Name : constant String := Path_Parsers.Element (URL, 1);
   From     : constant String := AWS.URL.Parameter (URL, From_Param);
   Count_S  : constant String := AWS.URL.Parameter (URL, Count_Param);
   Count    : constant Positive := Count_String_To_Positive (Count_S);
   Rev      : constant Boolean := AWS.URL.Parameter (URL, Reverse_Param) = "1";
   Incl     : constant Boolean := AWS.URL.Parameter (URL, Incl_Param) = "1";
begin
   if Map_Name = "" then
      Success := False;
      return;
   end if;

   declare
      Key : constant DB.Maps.Key_Type := Make_Key (From, Max => Rev);

      function Lower_Bound return DB.Maps.Bound_Type is
      begin
         case Rev is
            when False =>
               case Incl is
                  when False =>
                     return DB.Maps.New_Bound (DB.Maps.Greater, Key);
                  when True =>
                     return DB.Maps.New_Bound (DB.Maps.Greater_Or_Equal, Key);
               end case;
            when True =>
               return DB.Maps.Negative_Infinity_Bound;
         end case;
      end Lower_Bound;

      function Upper_Bound return DB.Maps.Bound_Type is
      begin
         case Rev is
            when False =>
               return DB.Maps.Positive_Infinity_Bound;
            when True =>
               case Incl is
                  when False =>
                     return DB.Maps.New_Bound (DB.Maps.Less, Key);
                  when True =>
                     return DB.Maps.New_Bound (DB.Maps.Less_Or_Equal, Key);
               end case;
         end case;
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
end Query_Map;

