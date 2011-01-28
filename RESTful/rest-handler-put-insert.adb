-- Abstract:
--
-- Inserts a key/value pair into the map.
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with AWS.Messages;
with AWS.Status;
with AWS.Response;
with AWS.URL;

with DB.Maps;
with DB.Types.Keys;
with DB.Types.Values;
with DB.Types.Times;

with REST.Input_Formats;
with REST.Log;
with REST.Maps;
with REST.Path_Parsers;

separate (REST.Handler.Put)
procedure Insert
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
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

   type String_Ref_Type is access all String;

   Map_Name   : constant String := Next_Path_Element;
   Global_Row : aliased String := Next_Path_Element;
begin
   if Map_Name = "" then
      Success := False;
      return;
   end if;

   declare
      use type DB.Maps.State_Type;

      Time  : constant DB.Types.Times.Time_Type := DB.Types.Times.Now; 
      Map   : constant Maps.Map_Ref_Type := Maps.Map_By_Name (Map_Name);
      State : DB.Maps.State_Type;

      type Handler_Type is new Ada.Finalization.Limited_Controlled and
         Input_Formats.Handler_Type with
         record
            Level  : Natural := 0;
            Row    : String_Ref_Type := Global_Row'Access;
            Column : String_Ref_Type := null;
         end record;

      overriding
      procedure Finalize (Handler : in out Handler_Type);

      overriding
      procedure Start_Anonymous_Object (Handler : in out Handler_Type);

      overriding
      procedure Start_Object
        (Handler : in out Handler_Type;
         Key     : in     String);

      overriding
      procedure End_Object (Handler : in out Handler_Type);

      overriding
      procedure Start_Anonymous_Array (Handler : in out Handler_Type);

      overriding
      procedure Start_Array
        (Handler : in out Handler_Type;
         Key     : in     String);

      overriding
      procedure End_Array (Handler : in out Handler_Type);

      overriding
      procedure Anonymous_Value
        (Handler : in out Handler_Type;
         Val     : in     DB.Types.Values.Value_Type);

      overriding
      procedure Value
        (Handler : in out Handler_Type;
         Key     : in     String;
         Val     : in     DB.Types.Values.Value_Type);

      overriding
      procedure Error (Handler : in out Handler_Type);


      procedure Free_Row (Handler : in out Handler_Type'Class)
      is
         procedure Free is new Ada.Unchecked_Deallocation
           (String, String_Ref_Type);
      begin
         if Handler.Row /= null and Handler.Row /= Global_Row'Access then
            Free (Handler.Row);
         end if;
      end Free_Row;


      procedure Free_Column (Handler : in out Handler_Type'Class)
      is
         procedure Free is new Ada.Unchecked_Deallocation
           (String, String_Ref_Type);
      begin
         if Handler.Column /= null then
            Free (Handler.Column);
         end if;
      end Free_Column;


      procedure Finalize (Handler : in out Handler_Type) is
      begin
         Free_Row (Handler);
         Free_Column (Handler);
      end Finalize;


      procedure Start_Anonymous_Object (Handler : in out Handler_Type) is
      begin
         Handler.Level := Handler.Level + 1;
      end Start_Anonymous_Object;


      procedure Start_Object
        (Handler : in out Handler_Type;
         Key     : in     String) is
      begin
         if Handler.Level = 0 then
            raise Malformed_Input_Data_Error;
         end if;
         Handler.Free_Row;
         Handler.Row   := new String'(Key);
         Handler.Level := Handler.Level + 1;
      end Start_Object;


      procedure End_Object (Handler : in out Handler_Type) is
      begin
         Handler.Level := Handler.Level - 1;
      end End_Object;


      procedure Start_Anonymous_Array (Handler : in out Handler_Type) is
      begin
         Handler.Level := Handler.Level + 1;
      end Start_Anonymous_Array;


      procedure Start_Array
        (Handler : in out Handler_Type;
         Key     : in     String) is
      begin
         if Handler.Level = 0 then
            raise Malformed_Input_Data_Error;
         end if;
         Handler.Free_Column;
         Handler.Column := new String'(Key);
         Handler.Level  := Handler.Level + 1;
      end Start_Array;


      procedure End_Array (Handler : in out Handler_Type) is
      begin
         Handler.Level := Handler.Level - 1;
      end End_Array;


      procedure Anonymous_Value
        (Handler : in out Handler_Type;
         Val     : in     DB.Types.Values.Value_Type) is
      begin
         -- Anonymous_Value only appears in arrays whereas Value appears only in
         -- objects.
         if Handler.Row = null then
            raise Malformed_Input_Data_Error;
         end if;
         if Handler.Column = null then
            raise Malformed_Input_Data_Error;
         end if;
         declare
            Key : constant DB.Types.Keys.Key_Type :=
              Make_Key (Handler.Row.all, Handler.Column.all, Time);
         begin
            Map.Append (Key, Val, State);
            if State /= DB.Maps.Success then
               raise Insertion_Error;
            end if;
         end;
      end Anonymous_Value;


      procedure Value
        (Handler : in out Handler_Type;
         Key     : in     String;
         Val     : in     DB.Types.Values.Value_Type) is
      begin
         if Handler.Row = null then
            raise Malformed_Input_Data_Error;
         end if;
         declare
            Col : String renames Key;
            Key : constant DB.Types.Keys.Key_Type :=
              Make_Key (Handler.Row.all, Col, Time);
         begin
            Map.Replace (Key, Val, State);
            if State /= DB.Maps.Success then
               raise Insertion_Error;
            end if;
         end;
      end Value;


      procedure Error (Handler : in out Handler_Type) is
      begin
         raise Malformed_Input_Data_Error;
      end Error;

      Parser  : Input_Formats.Parser_Type'Class :=
         Input_Formats.New_Parser (Request);
      Handler : Handler_Type;
   begin
      Input_Formats.Parse (Request, Parser, Handler);
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
end Insert;

