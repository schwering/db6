with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Random; use Random;
with This_Computer;
with To_Strings;

with DB;
with DB.IO.Blocks;

with DB.BTrees;
with DB.BTrees.Stats;
with DB.Gen_BTrees.Gen_Check;

with DB.Types;
with DB.Types.Keys;
with DB.Types.Values;

with DB.Utils.Timers; use DB.Utils.Timers;
with DB.Utils.Traceback;


procedure Tree
is
   package BTrees renames DB.BTrees;

   --      500_000 =>  94 MB
   --    5_000_000 =>  ~1 GB
   --   50_000_000 => ~10GB
   --  100_000_000 => ~20GB
   INSERT_COUNT   : constant := 5_000_000;
   DELETE_COUNT   : constant := INSERT_COUNT * 5 / 5;
   REINSERT_COUNT : constant := DELETE_COUNT;
   CHECK_INTERVAL : constant := INSERT_COUNT / 5;
   PRINT_INTERVAL : constant := INSERT_COUNT / 4;
   FILE_NAME      : constant String := This_Computer.BTree_File;

   Stop_Now : exception;

   subtype Key_Type is DB.Types.Keys.Key_Type;
   subtype Value_Type is DB.Types.Values.Value_Type;
   use type Key_Type;
   use type Value_Type;

   procedure Check (Tree : in out BTrees.Tree_Type)
   is
      procedure Check_Proc is
         new BTrees.Gen_Check(Key_To_String     => To_Strings.To_String,
                              Value_To_String   => To_Strings.To_String,
                              Address_To_String => To_Strings.To_String);
   begin
      Check_Proc(Tree);
      Put_Line("Check successful");
   exception
      when Error : others =>
         Put_Line("Check failed");
         Put_Line("Exception: "& Exception_Message(Error));
         raise Stop_Now;
   end Check;


   procedure Make_Stats (Tree : in out BTrees.Tree_Type)
   is
      Count                                    : BTrees.Count_Type;
      Height, Blocks, Free_Blocks, Used_Blocks : Natural;
      Max_Degree, Min_Degree, Avg_Degree       : Natural;
      Waste, Waste_Per_Block, Bytes            : Long_Integer;
      Relative_Waste_Per_Block                 : Float;
   begin
      BTrees.Stats(Tree                   => Tree,
                    Height                 => Height,
                    Blocks                 => Blocks,
                    Free_Blocks            => Free_Blocks,
                    Max_Degree             => Max_Degree,
                    Min_Degree             => Min_Degree,
                    Avg_Degree             => Avg_Degree,
                    Bytes_Wasted_In_Blocks => Waste,
                    Bytes_In_Blocks        => Bytes);
      BTrees.Count(Tree, Count);
      Used_Blocks              := Blocks - Free_Blocks;
      if Used_Blocks > 0 then
         Waste_Per_Block       := Waste / Long_Integer(Used_Blocks);
      else
         Waste_Per_Block       := 0;
      end if;
      Relative_Waste_Per_Block := Float(Waste_Per_Block)
                                / Float(DB.IO.Blocks.Block_Size);
      -- The lines contain the following
      -- "OK" Count Height Blocks Used_Blocks Free_Blocks Max_Deg Avg_Dev\
      -- Min_Deg Total_Waste Total_Sizes Waste_per_Block Rel_Waste_per_Block
      Put("OK");
      Put(BTrees.Count_Type'Image(Count));
      Put(Natural'Image(Height));
      Put(Natural'Image(Blocks));
      Put(Natural'Image(Used_Blocks));
      Put(Natural'Image(Free_Blocks));
      Put(Natural'Image(Max_Degree));
      Put(Natural'Image(Avg_Degree));
      Put(Natural'Image(Min_Degree));
      Put(Long_Integer'Image(Waste));
      Put(Long_Integer'Image(Bytes));
      Put(Long_Integer'Image(Waste_Per_Block));
      Put(Float'Image(Relative_Waste_Per_Block));
      New_Line;
   end;


   procedure Print (S : in String; Timer : in out DB.Utils.Timers.Timer_Type)
   is
      CPU_Duration  : constant Ticks_Type := CPU_Ticks(Timer);
      Real_Duration : constant Time_Type := Real_Time(Timer);
   begin
      Put(S &":");
      Put(Ticks_Type'Image(CPU_Duration));
      --Put(To_String(Real_Duration));
      Put(Time_Type'Image(Real_Duration));
      New_Line;
      --Reset(Timer);
   end Print;


   Tree_Timer : Timer_Type;
   Long_Timer : Timer_Type;
   Total_Timer : Timer_Type;

   use type BTrees.State_Type;
   Tree : BTrees.Tree_Type;
   Cnt  : BTrees.Count_Type;
begin
   declare
   begin
      BTrees.Create(FILE_NAME);
   exception
      when DB.IO_Error => null;
   end;
   BTrees.Initialize(Tree, FILE_NAME);
   BTrees.Count(Tree, Cnt);

   Init_Key_Value_Pairs(Count_Type(Cnt)*10+1);
   Put_Line("Size ="& BTrees.Count_Type'Image(Cnt));
   Put_Line("Init ="& Count_Type'Image(Count_Type(Cnt)*10+1));

   Start(Total_Timer);
   Check(Tree);
   Make_Stats(Tree);
   New_Line; Put_Line("INSERTING.");
   Reset_String_Generation;
   Reset(Tree_Timer);
   Reset(Long_Timer);
   Start(Long_Timer);
   declare
      use type BTrees.Count_Type;
      use type DB.IO.Blocks.Size_Type;
      Key    : Key_Type;
      Value  : Value_Type;
      Pos    : BTrees.Count_Type;
      State  : BTrees.State_Type;
   begin
      BTrees.Minimum(Tree, Key, Value, Pos, State);
      if State = BTrees.Error then
         Put_Line("Minimum failed");
      end if;
      BTrees.Maximum(Tree, Key, Value, Pos, State);
      if State = BTrees.Error then
         Put_Line("Maximum failed");
      end if;
   end;
   for I in 1 .. INSERT_COUNT loop
      declare
         use type BTrees.Count_Type;
         use type DB.IO.Blocks.Size_Type;
         KV     : constant Key_Value_Type := Random_Entry;
         Pos    : BTrees.Count_Type;
         State  : BTrees.State_Type;
         --Pos2   : BTrees.Count_Type;
         --Key2   : Key_Type;
         --Value2 : Value_Type;
      begin
         Start(Tree_Timer);
         BTrees.Insert(Tree, KV.Key, KV.Value, Pos, State);
         Stop(Tree_Timer);

         --BTrees.Delete(Tree, Pos, Value2, Key2, State);
         --if State /= BTrees.Success
            --or else KV.Key /= Key2
            --or else KV.Value /= Value2 then
            --Put_Line("Deletion failed -2");
         --end if;

         --BTrees.Look_Up(Tree, KV.Key, Value2, Pos2, State);
         --if State = BTrees.Success then
            --Put_Line("Deletion must have failed -1");
         --end if;

         --BTrees.Insert(Tree, KV.Key, KV.Value, Pos, State);
         --if State /= BTrees.Success then
            --Put_Line("New insertion failed");
         --end if;

         --BTrees.Update(Tree, Pos, 23, Value2, Key2, State);
         --if State /= BTrees.Success
            --or else KV.Value /= Value2
            --or else KV.Key /= Key2 then
            --Put_Line("Insertion not successful 0 "&
               --BTrees.State_Type'Image(State));
         --end if;


         --BTrees.Look_Up(Tree, KV.Key, Value2, Pos2, State);
         --if State /= BTrees.Success
            --or else 23 /= Value2
            --or else Pos /= Pos2 then
            --Put_Line("Insertion not successful 1 "&
               --BTrees.State_Type'Image(State));
         --end if;

         --BTrees.Update(Tree, Pos, KV.Value, Value2, Key2, State);
         --if State /= BTrees.Success
            --or else KV.Key /= Key2
            --or else Value2 /= 23 then
            --Put_Line("Insertion not successful 1.5 "&
               --BTrees.State_Type'Image(State));
         --end if;


         --BTrees.Look_Up(Tree, Pos, Value2, Key2, State);
         --if State /= BTrees.Success
            --or else KV.Value /= Value2
            --or else KV.Key /= Key2 then
            --Put_Line("Insertion not successful 2 "&
               --BTrees.State_Type'Image(State));
         --end if;

         if I mod PRINT_INTERVAL = 0 then
            Print(Integer'Image(I), Tree_Timer);
         elsif I mod CHECK_INTERVAL = 0 then
            Print(Integer'Image(I), Tree_Timer);
            Check(Tree);
            Make_Stats(Tree);
         end if;
      end;
   end loop;
   Stop(Long_Timer);
   Print("Long", Long_Timer);
   if True then
      return;
   end if;



   New_Line; Put_Line("SEARCH.");
   Reset_String_Generation;
   Reset(Tree_Timer);
   Reset(Long_Timer);
   Start(Long_Timer);
   for I in 1 .. INSERT_COUNT loop
      declare
         KV    : constant Key_Value_Type := Random_Entry;
         Value : Value_Type;
         Pos   : BTrees.Count_Type;
         State : BTrees.State_Type;
      begin
         Start(Tree_Timer);
         BTrees.Look_Up(Tree, KV.Key, Value, Pos, State);
         Stop(Tree_Timer);
         if State /= BTrees.Success then
            Put_Line("Failed"& BTrees.State_Type'Image(State)
                    &" at"& Integer'Image(I));
            return;
         end if;
         if KV.Value /= Value then
            Put_Line("Failed"& BTrees.State_Type'Image(State)
                    &" at"& Integer'Image(I) &" 2");
            return;
         end if;
         if I mod PRINT_INTERVAL = 0 then
            Print(Integer'Image(I), Tree_Timer);
         end if;
         declare
            M_Key   : Key_Type;
            M_Value : Value_Type;
            M_Pos   : BTrees.Count_Type;
            M_State : BTrees.State_Type;
         begin
            BTrees.Minimum(Tree, M_Key, M_Value, M_Pos, M_State);
            if M_State /= BTrees.Success or else not (M_Key <= KV.Key) then
               Put_Line("Minimum incorrect");
            end if;
            BTrees.Maximum(Tree, M_Key, M_Value, M_Pos, M_State);
            if M_State /= BTrees.Success or else not (KV.Key <= M_Key) then
               Put_Line("Maximum incorrect");
            end if;
         end;
      end;
   end loop;
   Stop(Long_Timer);
   Print("Long", Long_Timer);


   New_Line; Put_Line("DELETING.");
   Reset_String_Generation;
   Reset(Tree_Timer);
   Reset(Long_Timer);
   Start(Long_Timer);
   for I in 1 .. DELETE_COUNT loop
      declare
         KV    : constant Key_Value_Type := Random_Entry;
         Value : Value_Type;
         Pos   : BTrees.Count_Type;
         State : BTrees.State_Type;
      begin
         Start(Tree_Timer);
         BTrees.Delete(Tree, KV.Key, Value, Pos, State);
         Stop(Tree_Timer);
         if State /= BTrees.Success then
            Put_Line("Failed"& BTrees.State_Type'Image(State)
                    &" at"& Integer'Image(I));
            return;
         end if;
         if KV.Value /= Value then
            Put_Line("Failed"& BTrees.State_Type'Image(State)
                    &" at"& Integer'Image(I) &" 2");
            return;
         end if;
         if I mod PRINT_INTERVAL = 0 then
            Print(Integer'Image(I), Tree_Timer);
         elsif I mod CHECK_INTERVAL = 0 then
            Check(Tree);
            Make_Stats(Tree);
         end if;
      end;
   end loop;
   Stop(Long_Timer);
   Print("Long", Long_Timer);


   --New_Line; Put_Line("ANTIUPDATE.");
   --Reset_String_Generation;
   --Reset(Tree_Timer);
   --Reset(Long_Timer);
   --Start(Long_Timer);
   --for I in 1 .. DELETE_COUNT loop
      --declare
         --KV        : constant Key_Value_Type := Random_Entry;
         --Value     : Value_Type;
         --New_Value : Value_Type;
         --Old_Value : Value_Type;
         --Pos       : BTrees.Count_Type;
         --State     : BTrees.State_Type;
         --use type DB.IO.Blocks.Direct_IO.Valid_Address_Type;
      --begin
         --BTrees.Look_Up(Tree, KV.Key, Value, Pos, State);
         --if State /= BTrees.Failure then
            --Put_Line("Failed"& BTrees.State_Type'Image(State)
                    --&" at"& Integer'Image(I));
            --return;
         --end if;
         --New_Value := Value * 1000 / 23;
         --Start(Tree_Timer);
         --BTrees.Update(Tree, KV.Key, New_Value, Old_Value, Pos, State);
         --Stop(Tree_Timer);
         --if State /= BTrees.Failure then
            --Put_Line("Antiupdate Failed"& BTrees.State_Type'Image(State)
                    --&" at"& Integer'Image(I));
            --return;
         --end if;
         --if I mod PRINT_INTERVAL = 0 then
            --Print(Integer'Image(I), Tree_Timer);
         --end if;
      --end;
   --end loop;
   --New_Line; Put_Line("UPDATE.");
   --Reset(Tree_Timer);
   --Reset(Long_Timer);
   --Start(Long_Timer);
   --for I in 1 .. DELETE_COUNT loop
      --declare
         --KV        : constant Key_Value_Type := Random_Entry;
         --Value     : Value_Type;
         --New_Value : Value_Type;
         --Old_Value : Value_Type;
         --Pos       : BTrees.Count_Type;
         --State     : BTrees.State_Type;
      --begin
         --BTrees.Look_Up(Tree, KV.Key, Value, Pos, State);
         --if State /= BTrees.Success then
            --Put_Line("Failed"& BTrees.State_Type'Image(State)
                    --&" at"& Integer'Image(I));
            --return;
         --end if;
         --if KV.Value /= Value then
            --Put_Line("Failed"& BTrees.State_Type'Image(State)
                    --&" at"& Integer'Image(I) &" 2");
            --return;
         --end if;
         --New_Value := Value * 1000 / 23;
         --Start(Tree_Timer);
         --BTrees.Update(Tree, KV.Key, New_Value, Old_Value, Pos, State);
         --Stop(Tree_Timer);
         --if State /= BTrees.Success then
            --Put_Line("Update Failed"& BTrees.State_Type'Image(State)
                    --&" at"& Integer'Image(I));
            --return;
         --end if;
         --if Old_Value /= Value then
            --Put_Line("Old_Value /= Value at"& Integer'Image(I));
         --end if;
         --BTrees.Look_Up(Tree, KV.Key, Value, Pos, State);
         --if New_Value /= Value then
            --Put_Line("New_Value /= Value at"& Integer'Image(I));
         --end if;
         --if I mod PRINT_INTERVAL = 0 then
            --Print(Integer'Image(I), Tree_Timer);
         --end if;
      --end;
   --end loop;
   --Stop(Long_Timer);
   --Print("Long", Long_Timer);


   New_Line; Put_Line("ANTISEARCH.");
   Reset_String_Generation;
   Reset(Long_Timer);
   Start(Long_Timer);
   for I in 1 .. DELETE_COUNT loop
      declare
         KV    : constant Key_Value_Type := Random_Entry;
         Value : Value_Type;
         Pos   : BTrees.Count_Type;
         State : BTrees.State_Type;
      begin
         BTrees.Look_Up(Tree, KV.Key, Value, Pos, State);
         if State /= BTrees.Failure then
            Put_Line("False Positive"& BTrees.State_Type'Image(State)
                    &" at"& Integer'Image(I));
            Put_Line("False Positive: "& Value_Type'Image(Value));
            return;
         end if;
      end;
   end loop;
   Stop(Long_Timer);
   Print("Long", Long_Timer);


   Check(Tree);


   New_Line; Put_Line("REINSERTING.");
   Reset_String_Generation;
   Reset(Tree_Timer);
   Reset(Long_Timer);
   Start(Long_Timer);
   for I in 1 .. REINSERT_COUNT loop
      declare
         KV    : constant Key_Value_Type := Random_Entry;
         Pos   : BTrees.Count_Type;
         State : BTrees.State_Type;
      begin
         Start(Tree_Timer);
         BTrees.Insert(Tree, KV.Key, KV.Value, Pos, State);
         Stop(Tree_Timer);
         if I mod PRINT_INTERVAL = 0 then
            Print(Integer'Image(I), Tree_Timer);
         elsif I mod CHECK_INTERVAL = 0 then
            Check(Tree);
            Make_Stats(Tree);
         end if;
      end;
   end loop;
   Stop(Long_Timer);
   Print("Long", Long_Timer);


   New_Line; Put_Line("SEARCH.");
   Reset_String_Generation;
   Reset(Tree_Timer);
   Reset(Long_Timer);
   Start(Long_Timer);
   for I in 1 .. REINSERT_COUNT loop
      declare
         KV      : constant Key_Value_Type := Random_Entry;
         Value   : Value_Type;
         Pos     : BTrees.Count_Type;
         State   : BTrees.State_Type;
      begin
         Start(Tree_Timer);
         BTrees.Look_Up(Tree, KV.Key, Value, Pos, State);
         Stop(Tree_Timer);
         if State /= BTrees.Success then
            Put_Line("Failed"& BTrees.State_Type'Image(State)
                    &" at"& Integer'Image(I));
            return;
         end if;
         if I mod PRINT_INTERVAL = 0 then
            Print(Integer'Image(I), Tree_Timer);
         end if;
      end;
   end loop;
   Stop(Long_Timer);
   Print("Long", Long_Timer);

   Check(Tree);
   Make_Stats(Tree);


   Print(Integer'Image(REINSERT_COUNT), Tree_Timer);
   Make_Stats(Tree);

   BTrees.Finalize(Tree);

   Put_Line("Finished.");
   Stop(Total_Timer);
   Print("Total", Total_Timer);

exception
   when Stop_Now =>
      null;
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Utils.Traceback.Print_Traceback;
      Put_Line("Traceback");
      DB.Utils.Traceback.Print_Traceback(Error);
      Put_Line("Traceback");

end Tree;

