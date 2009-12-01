with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with IO_Dispatcher.Random; use IO_Dispatcher.Random;
with IO_Dispatcher.Args;
with IO_Dispatcher.Jobs;
with IO_Dispatcher.Gen_Simple_Jobs;
with IO_Dispatcher.To_Strings; use IO_Dispatcher.To_Strings;

with DB;
with DB.IO.Blocks;

with DB.Tables.Maps;

with DB.Types.Keys;
with DB.Types.Strings;
with DB.Types.Strings.Bounded;
with DB.Types.Values.Bounded;
with DB.Types.Values.Unbounded;
with DB.Types.Times;

with DB.Utils.Traceback;

procedure IO_Dispatcher.MMap is
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


   procedure Check_Key_Value (KV : Key_Value_Type)
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
   end Check_Key_Value;


   function To_String (V : DB.Tables.Value_Type'Class) return String is
   begin
      if V in Value_Type'Class then
         declare
            Len : constant DB.Types.Values.Bounded.Length_Type
                := DB.Types.Values.Bounded.Length(Value_Type(V).S);
         begin
            return "'"& To_String(Value_Type(V).S) &"' ["& Len'Img &"]";
         end;
      else
         return "wrong instance";
      end if;
   end To_String;


   procedure Make_Stats
     (Tree                   : in out DB.Tables.Maps.Map_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer) is null;
   procedure Check (Tree : in out DB.Tables.Maps.Map_Type) is null;



   package Simple_Jobs is new Gen_Simple_Jobs
     (Object_Type     => DB.Tables.Maps.Map_Type,
      Key_Type        => DB.Types.Keys.Key_Type,
      Value_Type      => DB.Tables.Value_Type'Class,
 
      Key_To_String   => To_String,
      Value_To_String => To_String,

      "="             => DB.Tables."=",

      Check_Key_Value => Check_Key_Value,

      Key_Value_Type  => Random.Key_Value_Type,
      Random_Entry    => Random.Random_Entry,
      Get_Key         => Random.Key,
      Get_Value       => Get_Value,

      Count_Type      => DB.Tables.Maps.Count_Type,
      Result_Type     => DB.Tables.Maps.Result_Type,

      Object          => Map,
      Null_Value      => Null_Value,
      Success         => DB.Tables.Maps.Success,
      Failure         => DB.Tables.Maps.Failure,

      P_Insert        => DB.Tables.Maps.Insert,
      P_Delete        => DB.Tables.Maps.Delete,
      P_Look_Up       => DB.Tables.Maps.Look_Up,
      P_Count         => DB.Tables.Maps.Count,
      P_Make_Stats    => Make_Stats,
      P_Check         => Check);


   procedure Map_Reduce
   is
      type Element_Type is
         record
            Key : DB.Types.Keys.Key_Type;
         end record;
      Neutral_Element : constant Element_Type
          := Element_Type'
                  (Key => DB.Types.Keys.Key_Type'
                              (Row    => DB.Types.Keys.Rows.Empty_String,
                               Column => DB.Types.Keys.Rows.Empty_String,
                               Time   => 0));

      I : Integer := 0;

      function Map_Function (Key   : DB.Types.Keys.Key_Type;
                             Value : DB.Tables.Value_Type'Class)
                             return Element_Type is
      begin
         declare
            task Delay_Task;
            task body Delay_Task is begin delay 0.1; end Delay_Task;
         begin
            null;
         end;
         Put_Line(I'Img);
         return Element_Type'(Key => Key);
      end Map_Function;

      procedure Reduce (Left  : in out Element_Type;
                        Right : in     Element_Type)
      is
         use type DB.Types.Keys.Key_Type;
      begin
         I := I + 1;
         if Right.Key <= Left.Key then
            Put_Line(I'Img &" Less-than does not hold!");
         end if;
      end Reduce;

      procedure Map_Reduce is new DB.Tables.Maps.Gen_Sequential_Map_Reduce
        (Element_Type, Neutral_Element, Map_Function, Reduce);

      Transaction : DB.Tables.Maps.RO_Transaction_Type
                  := DB.Tables.Maps.New_RO_Transaction(Map);
      Lower       : constant DB.Tables.Maps.Bound_Type
                  := DB.Tables.Maps.Negative_Infinity_Bound(Map);
      Upper       : constant DB.Tables.Maps.Bound_Type
                  := DB.Tables.Maps.Positive_Infinity_Bound(Map);
      Cursor      : DB.Tables.Maps.Cursor_Type
                  := DB.Tables.Maps.New_Cursor(Map, Transaction, True,
                                               Lower, Upper, False);
      Element     : Element_Type;
      Value_Impl  : Value_Type;
      State       : DB.Tables.Maps.Result_Type;
      use type DB.Tables.Maps.Result_Type;
   begin
      Put_Line("MapReduce");
      DB.Tables.Maps.Start_Transaction(Map, Transaction);
      Map_Reduce(Map, Transaction, Cursor, Element, Value_Impl, State);
      Put_Line("State = "& State'Img);
      if State /= DB.Tables.Maps.Success then
         Put_Line("State = "& State'Img);
      end if;
      DB.Tables.Maps.Finalize(Map, Cursor);
      DB.Tables.Maps.Finish_Transaction(Map, Transaction);
   exception
      when others =>
         DB.Tables.Maps.Finish_Transaction(Map, Transaction);
         raise;
   end Map_Reduce;


   use type DB.Tables.Maps.Result_Type;
   Job_Map  : constant Jobs.Map_Type
            := Jobs.Add(Simple_Jobs.Job_Map, "MapReduce",
                        Map_Reduce'Unrestricted_Access);
   Long_Job : constant Jobs.Long_Job_Type
            := Args.Create_Jobs_From_Command_Line(Job_Map);
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
exception
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Utils.Traceback.Print_Traceback(Error);
end IO_Dispatcher.MMap;

