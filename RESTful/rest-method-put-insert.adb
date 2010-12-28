-- Abstract:
--
-- Inserts a key/value pair into the map.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Strings.Unbounded;

with AWS.Messages;
with AWS.Status;
with AWS.Response;
with AWS.URL;

with DB.Maps.Values.Booleans;
with DB.Maps.Values.Long_Floats;
with DB.Maps.Values.Long_Integers;
with DB.Maps.Values.Nothings;
with DB.Maps.Values.Strings;

with REST.Maps;
with REST.Path_Parsers;

separate (REST.Method.Put)
procedure Insert
   (Request  : in  AWS.Status.Data;
    Response : out AWS.Response.Data;
    Success  : out Boolean)
is
   procedure To_Boolean
     (Value   : in  String;
      Bool    : out DB.Maps.Values.Booleans.Value_Type;
      Success : out Boolean) is
   begin
      Bool := DB.Maps.Values.Booleans.New_Value
        (DB.Maps.Values.Booleans.Integer_Type'Value (Value));
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end To_Boolean;

   procedure To_Int
     (Value   : in  String;
      Int     : out DB.Maps.Values.Long_Integers.Value_Type;
      Success : out Boolean) is
   begin
      Int := DB.Maps.Values.Long_Integers.New_Value
        (DB.Maps.Values.Long_Integers.Integer_Type'Value (Value));
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end To_Int;

   procedure To_Float
     (Value   : in  String;
      Flt     : out DB.Maps.Values.Long_Floats.Value_Type;
      Success : out Boolean) is
   begin
      Flt := DB.Maps.Values.Long_Floats.New_Value
        (DB.Maps.Values.Long_Floats.Float_Type'Value (Value));
      Success := True;
   exception
      when Constraint_Error =>
         Success := False;
   end To_Float;

   procedure To_String
     (Value   : in  String;
      Str     : out DB.Maps.Values.Strings.Value_Type;
      Success : out Boolean) is
   begin
      if Value'Length = 0 or else
         (Value (Value'First) /= '"' or Value (Value'Last) /= '"')
      then
         Success := False;
      else
         Str := DB.Maps.Values.Strings.New_Value
           (Value (Value'First + 1 .. Value'Last - 1));
         Success := True;
      end if;
   end To_String;

   procedure To_Nothing
     (Value   : in  String;
      Success : out Boolean) is
   begin
      Success := Value = "null";
   end To_Nothing;

   function To_Value (Value : String) return DB.Maps.Value_Type'Class
   is
      Success : Boolean;
   begin
      declare
         S : DB.Maps.Values.Strings.Value_Type;
      begin
         To_String (Value, S, Success);
         if Success then
            return S;
         end if;
      end;

      declare
         F : DB.Maps.Values.Long_Floats.Value_Type;
      begin
         To_Float (Value, F, Success);
         if Success then
            return F;
         end if;
      end;

      declare
         I : DB.Maps.Values.Long_Integers.Value_Type;
      begin
         To_Int (Value, I, Success);
         if Success then
            return I;
         end if;
      end;

      declare
         B : DB.Maps.Values.Booleans.Value_Type;
      begin
         To_Boolean (Value, B, Success);
         if Success then
            return B;
         end if;
      end;

      declare
         N : DB.Maps.Values.Nothings.Value_Type;
      begin
         To_Nothing (Value, Success);
         if Success then
            return N;
         end if;
      end;
      
      raise Invalid_Parameter_Error;
   end To_Value;

   URL      : constant AWS.URL.Object := AWS.Status.URI (Request);
   Map_Name : constant String := Path_Parsers.Element (URL, 1);
   Row      : constant String := AWS.URL.Parameter (URL, Row_Param);
   Col      : constant String := AWS.URL.Parameter (URL, Column_Param);
   Value    : constant String := AWS.URL.Parameter (URL, Value_Param);
begin
   if Map_Name = "" or Row = "" or Col = "" or Value = "" then
      Success := False;
      return;
   end if;

   declare
      use type DB.Maps.State_Type;
      Map : constant Maps.Map_Ref_Type := Maps.Map_By_Name (Map_Name);
      Key : constant DB.Maps.Keys.Key_Type := DB.Maps.Strings_To_Key (Row, Col);
      Val : constant DB.Maps.Value_Type'Class := To_Value (Value);
      St  : DB.Maps.State_Type;
   begin
      Map.Insert (Key, Val, St);
      case St is
         when DB.Maps.Success =>
            Response := AWS.Response.Build
              (Status_Code  => AWS.Messages.S200,
               Content_Type => "text/plain",
               Message_Body => "ok");
         when DB.Maps.Failure =>
            Response := AWS.Response.Build
              (Status_Code  => AWS.Messages.S500,
               Content_Type => "text/plain",
               Message_Body => "error");
      end case;
      Success := True;
   end;
end Insert;

