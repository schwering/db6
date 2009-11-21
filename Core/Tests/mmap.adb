-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Random; use Random;
with Args;
with Jobs;
with Gen_Simple_Jobs;
with To_Strings; use To_Strings;

with DB;
with DB.IO.Blocks;
with DB.IO.Blocks.File_IO;

with DB.Tables.Maps;

with DB.Types.Keys;
with DB.Types.Strings;
with DB.Types.Strings.Bounded;
with DB.Types.Values.Bounded;
with DB.Types.Values.Unbounded;
with DB.Types.Times;

with DB.Utils.Traceback;


procedure MMap is

   Max_Key_Size   : constant := 2 + 1000 + 8 
                                ;--+ 1; -- to enforce heaped map
   Max_Value_Size : constant := 8;

   Map : DB.Tables.Maps.Map_Type
       := DB.Tables.Maps.New_Map(Max_Key_Size, Max_Value_Size);

   ----------
   -- Value definition.

   type Value_Type is new DB.Tables.Value_Type with
      record
         S : DB.Types.Values.Bounded.String_Type;
      end record;

   overriding function "=" (Left, Right : Value_Type) return Boolean;
   overriding function To_Bounded (V : Value_Type)
      return DB.Types.Values.Bounded.String_Type;
   overriding function To_Unbounded (V : Value_Type)
      return DB.Types.Values.Unbounded.String_Type;
   overriding function From_Bounded (S : DB.Types.Values.Bounded.String_Type)
      return Value_Type;
   overriding function From_Unbounded (S: DB.Types.Values.Unbounded.String_Type)
      return Value_Type;

   overriding function "=" (Left, Right : Value_Type) return Boolean
   is
      use type DB.Types.Values.Bounded.String_Type;
   begin
      return Left.S = Right.S;
   end "=";

   overriding function To_Bounded
     (V : Value_Type)
      return DB.Types.Values.Bounded.String_Type
   is
      use type DB.Types.Values.Bounded.String_Type;
   begin
      return V.S;
   end To_Bounded;

   overriding function To_Unbounded
     (V : Value_Type)
      return DB.Types.Values.Unbounded.String_Type
   is
      B : constant DB.Types.Values.Indefinite_Buffer_Type
        := DB.Types.Values.Bounded.To_Buffer(V.S);
   begin
      return DB.Types.Values.Unbounded.New_String(B);
   end To_Unbounded;

   overriding function From_Bounded
     (S : DB.Types.Values.Bounded.String_Type)
      return Value_Type
   is
      use type DB.Types.Values.Bounded.String_Type;
      V : Value_Type;
   begin
      V.S := S;
      return V;
   end From_Bounded;

   overriding function From_Unbounded
     (S : DB.Types.Values.Unbounded.String_Type)
      return Value_Type
   is
      B : constant DB.Types.Values.Indefinite_Buffer_Type
        := DB.Types.Values.Unbounded.To_Buffer(S);
      V : Value_Type;
   begin
      V.S := DB.Types.Values.Bounded.New_String(B);
      return V;
   end From_Unbounded;


   function Get_Value
     (KV : Random.Key_Value_Type)
      return DB.Tables.Value_Type'Class
   is
      V : Value_Type;
   begin
      V.S := KV.Value;
      return V;
   end Get_Value;


   Null_Value : constant Value_Type
              := From_Bounded(DB.Types.Values.Bounded.Empty_String);


   procedure Check (KV : Key_Value_Type)
   is
      use DB.IO.Blocks;
      use DB.Types.Strings.Bounded;
      use DB.Types.Values.Bounded;
      use type Size_Type;

      function KS return DB.IO.Blocks.Size_Type is
      begin
         return 2 + Size_Type(Length(KV.Key.Row)) +
              --2 + Size_Type(Length(KV.Key.Column)) +
                Bits_To_Units(DB.Types.Times.Number_Type'Size);
      end;
      function VS return DB.IO.Blocks.Size_Type is
      begin
         return Size_Type(Length(KV.Value));
      end;
   begin
      pragma Assert (KS <= DB.Tables.Maps.Max_Key_Size(Map, VS));
      null;
   end Check;


   function To_String (V : DB.Tables.Value_Type'Class) return String is
   begin
      if V in Value_Type'Class then
         declare
            Len : constant DB.Types.Values.Bounded.Length_Type
                := DB.Types.Values.Bounded.Length(Value_Type(V).S);
         begin
            Put_Line("Checking");
            Put_Line("Checked: "& Boolean'Image(Value_Type(V) = Null_Value));
            return "'"& To_String(Value_Type(V).S) &"' ["& Len'Img &"]";
         end;
      else
         return "wrong instance";
      end if;
   end To_String;


   package Simple_Jobs is new Gen_Simple_Jobs
     (Object_Type    => DB.Tables.Maps.Map_Type,
      Key_Type       => DB.Types.Keys.Key_Type,
      Value_Type     => DB.Tables.Value_Type'Class,
      Check          => Check,
      To_String      => To_String,
      Key_Value_Type => Random.Key_Value_Type,
      Random_Entry   => Random.Random_Entry,
      Get_Key        => Random.Key,
      Get_Value      => Get_Value,
      Count_Type     => DB.Tables.Maps.Count_Type,
      Result_Type    => DB.Tables.Maps.Result_Type,
      Object         => Map,
      Null_Value     => Null_Value,
      Success        => DB.Tables.Maps.Success,
      Failure        => DB.Tables.Maps.Failure,
      P_Insert       => DB.Tables.Maps.Insert,
      P_Delete       => DB.Tables.Maps.Delete,
      P_Look_Up      => DB.Tables.Maps.Look_Up);


   use type DB.Tables.Maps.Result_Type;
   Long_Job : constant Jobs.Long_Job_Type
            := Args.Create_Jobs_From_Command_Line(Simple_Jobs.Job_Map);
   Cnt      : DB.Tables.Maps.Count_Type := 0;
begin
   declare
   begin
      DB.Tables.Maps.Create(Args.File_Name, Max_Key_Size, Max_Value_Size);
      Put_Line("Newly created Map "& Args.File_Name);
   exception
      when DB.IO_Error => Put_Line("Using existing Map "& Args.File_Name);
   end;
   DB.Tables.Maps.Initialize(Map, Args.File_Name);
   DB.Tables.Maps.Count(Map, Cnt);
   Put_Line("Size ="& DB.Tables.Maps.Count_Type'Image(Cnt));

   declare
      RC : constant Random.Count_Type := Random.Count_Type(Cnt);
      IO : constant Random.Count_Type := Args.Init_Offset;
      I  : constant Count_Type := (RC - Random.Count_Type'Min(RC, IO)) + 1;
   begin
      Init_Key_Value_Pairs(I);
      Put_Line("Init ="& Count_Type'Image(I));
   end;

   Jobs.Execute_Jobs(Long_Job);

   DB.Tables.Maps.Finalize(Map);

   if False then
      declare
         use DB.IO.Blocks.File_IO;
         File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create(File, Ada.Text_IO.Out_File, "block_access");
         for I in Blocks'Range loop
            Ada.Text_IO.Put_Line(File, I'Img &" = "& Blocks(I)'Img);
         end loop;
         Ada.Text_IO.Close(File);
      end;
   end if;

exception
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Utils.Traceback.Print_Traceback(Error);
end MMap;

