-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Random; use Random;
with To_Strings;
with Gen_Args;
with Gen_Jobs;

with DB.IO.Blocks;

with DB.Tables.Maps;

with DB.Types.Keys;
with DB.Types.Values;

with DB.Utils.Traceback;


procedure MMap
is
   package Maps renames DB.Tables.Maps;
   package Jobs is new Gen_Jobs;
   package Args is new Gen_Args(Jobs);

   subtype Key_Type is DB.Types.Keys.Key_Type;
   subtype Value_Type is DB.Types.Values.Value_Type;
   use type Key_Type;
   use type Value_Type;
   use type DB.IO.Blocks.Size_Type;

   Max_Key_Size   : constant := 2 + 1000 + 8 
                                ;--+ 1; -- to enforce heaped map
   Max_Value_Size : constant := 8;

   Map : Maps.Map_Type := Maps.New_Map(Max_Key_Size, Max_Value_Size);


   procedure Insert
   is
      use type Maps.Count_Type;
      use type Maps.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Pos    : Maps.Count_Type;
      State  : Maps.Result_Type;
   begin
      Maps.Insert(Map, KV.Key, KV.Value, Pos, State);
      if State /= Maps.Success then
         Put_Line("Insertion failed");
      end if;
   end Insert;


   procedure Delete
   is
      use type Maps.Count_Type;
      use type Maps.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Val    : Value_Type;
      Pos    : Maps.Count_Type;
      State  : Maps.Result_Type;
   begin
      Maps.Delete(Map, KV.Key, Val, Pos, State);
      if State /= Maps.Success or else KV.Value /= Val then
         Put_Line("Deletion failed");
      end if;
   end Delete;


   procedure Search
   is
      use type Maps.Count_Type;
      use type Maps.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Val    : Value_Type;
      Pos    : Maps.Count_Type;
      State  : Maps.Result_Type;
   begin
      Maps.Look_Up(Map, KV.Key, Val, Pos, State);
      if State /= Maps.Success or else KV.Value /= Val then
         Put_Line("Look up failed "& Maps.Result_Type'Image(State));
      end if;
   end Search;


   procedure Antisearch
   is
      use type Maps.Count_Type;
      use type Maps.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Val    : Value_Type;
      Pos    : Maps.Count_Type;
      State  : Maps.Result_Type;
   begin
      Maps.Look_Up(Map, KV.Key, Val, Pos, State);
      if State /= Maps.Failure then
         Put_Line("Look up failed");
      end if;
   end Antisearch;


   use type Maps.Result_Type;
   Job_Map  : constant Jobs.Map_Type
            := ((Jobs.To_Description("Insert"),     Insert'Access),
                (Jobs.To_Description("Delete"),     Delete'Access),
                (Jobs.To_Description("Search"),     Search'Access),
                (Jobs.To_Description("Antisearch"), Antisearch'Access));
   Long_Job : constant Jobs.Long_Job_Type
            := Args.Create_Jobs_From_Command_Line(Job_Map);
   Cnt      : Maps.Count_Type := 0;
begin
   declare
   begin
      Maps.Create(Args.File_Name, Max_Key_Size, Max_Value_Size);
      Put_Line("Newly created Map "& Args.File_Name);
   exception
      when DB.IO.IO_Error => Put_Line("Using existing Map "& Args.File_Name);
   end;
   Maps.Initialize(Map, Args.File_Name);
   Maps.Count(Map, Cnt);
   Put_Line("Size ="& Maps.Count_Type'Image(Cnt));

   declare
      RC : constant Random.Count_Type := Random.Count_Type(Cnt);
      IO : constant Random.Count_Type := Args.Init_Offset;
      I  : constant Count_Type := (RC - Random.Count_Type'Min(RC, IO)) * 10 + 1;
   begin
      Init_Key_Value_Pairs(I);
      Put_Line("Init ="& Count_Type'Image(I));
   end;

   Jobs.Execute_Jobs(Long_Job);

   Maps.Finalize(Map);

exception
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Utils.Traceback.Print_Traceback(Error);
end MMap;

