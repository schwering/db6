-- Abstract:
--
-- Queries a map.
--
-- Copyright 2008--2011 Christoph Schwering

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
   URL  : constant AWS.URL.Object := AWS.Status.URI (Request);
   Path : constant String := AWS.URL.Pathname (URL);
   Iter : Path_Parsers.Iterator_Type;

   function Get_Count (S : String) return Positive is
   begin
      if S = "" then
         return 1;
      elsif S = "inf" then
         return Positive'Last;
      else
         return Positive'Value (S);
      end if;
   exception
      when others =>
         return 1;
   end Get_Count;

   function Param (S : String) return String is
   begin
      return AWS.URL.Parameter (URL, S);
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
   Count       : constant Positive := Get_Count (Param (Count_Param));

   function Bound
     (Row       : String;
      Inclusive : Boolean;
      Lower     : Boolean)
      return DB.Maps.Bound_Type
   is
      function Infinity return DB.Maps.Bound_Type is
      begin
         if Lower then
            return DB.Maps.Negative_Infinity_Bound;
         else
            return DB.Maps.Positive_Infinity_Bound;
         end if;
      end Infinity;

      function Comparison return DB.Maps.Comparison_Type is
      begin
         case Lower is
            when True =>
               case Inclusive is
                  when True =>
                     return DB.Maps.Greater;
                  when False =>
                     return DB.Maps.Greater_Or_Equal;
               end case;
            when False =>
               case Inclusive is
                  when True =>
                     return DB.Maps.Less;
                  when False =>
                     return DB.Maps.Less_Or_Equal;
               end case;
         end case;
      end Comparison;

   begin
      if Row = Infinity_Row then
         return Infinity;
      else
         return DB.Maps.New_Bound (Comparison,
                                   Make_Key (Row, Max => not Lower));
      end if;
   end Bound;

begin
   if Map_Name = "" or Row_1 = "" or Col_Regexp = "" then
      Success := True;
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

      Map    : constant Maps.Map_Ref_Type := Maps.Map_By_Name (Map_Name);
      Cursor : constant DB.Maps.Cursor_Ref_Type := Map.New_Cursor_Ref
        (Thread_Safe   => False,
         Lower_Bound   => Bound (Row_1, Inclusive_1, Lower => True),
         Upper_Bound   => Bound (Row_2, Inclusive_2, Lower => False),
         Column_Regexp => Col_Regexp);
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

