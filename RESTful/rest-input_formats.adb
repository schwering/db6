-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;

with DB.Maps.Values;
with DB.Maps.Values.Booleans;
with DB.Maps.Values.Long_Floats;
with DB.Maps.Values.Long_Integers;
with DB.Maps.Values.Nothings;
with DB.Maps.Values.Strings;

package body REST.Input_Formats is

   procedure Read
     (Request : in     AWS.Status.Data;
      Buffer  :    out AWS.Status.Stream_Element_Array;
      Last    :    out AWS.Status.Stream_Element_Offset) is
   begin
      AWS.Status.Read_Body (Request, Buffer, Last);
   end Read;


   procedure Parse
     (Request : in     AWS.Status.Data;
      Parser  : in out Parser_Type'Class;
      Handler : in out Handler_Type'Class) is
   begin
      loop
         declare
            Token : Token_Type;
         begin
            Parser.Next_Token (Request, Token);
            case Token is
               when Anonymous_Object_Start => Handler.Start_Anonymous_Object;
               when Object_Start => Handler.Start_Object (Parser.Key);
               when Object_End   => Handler.End_Object;
               when Anonymous_Array_Start  => Handler.Start_Anonymous_Array;
               when Array_Start  => Handler.Start_Array (Parser.Key);
               when Array_End    => Handler.End_Array;
               when Value        => Handler.Value (Parser.Key, Parser.Value);
               when EOF          => exit;
            end case;
         end;
      end loop;
   end Parse;


   procedure Initialize (Parser : in out Parser_Type) is
   begin
      Parser.The_Key   := null;
      Parser.The_Value := null;
   end Initialize;


   procedure Finalize (Parser : in out Parser_Type)
   is
      use type DB.Maps.Value_Ref_Type;
      procedure Free is new Ada.Unchecked_Deallocation
        (String, String_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (DB.Maps.Value_Type'Class, DB.Maps.Value_Ref_Type);
   begin
      if Parser.The_Key /= null then
         Free (Parser.The_Key);
      end if;
      if Parser.The_Value /= null then
         Free (Parser.The_Value);
      end if;
   end Finalize;


   procedure Set_Key
     (Parser : in out Parser_Type;
      Key    : in     String) is
   begin
      Parser.The_Key := new String'(Key);
   end Set_Key;


   procedure Set_Value
     (Parser : in out Parser_Type;
      Value  : in     String)
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

   begin
      Parser.The_Value := new DB.Maps.Value_Type'Class'(To_Value (Value));
   end Set_Value;


   function Key (Parser : Parser_Type) return String is
   begin
      return Parser.The_Key.all;
   end Key;


   function Value (Parser : Parser_Type) return DB.Maps.Value_Type'Class is
   begin
      return Parser.The_Value.all;
   end Value;

end REST.Input_Formats;

