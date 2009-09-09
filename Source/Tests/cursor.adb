-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Random; use Random;
with This_Computer;
with To_Strings;

with DB.IO.Blocks;

with DB.Volatile_BTrees;
with DB.Gen_BTrees.Gen_Check;
with DB.Volatile_BTrees.Stats;

with DB.Types;
with DB.Types.Rows;
with DB.Types.Columns;
with DB.Types.Times;
with DB.Types.Keys;
with DB.Types.Values;

with DB.Util.Timers;
with DB.Util.Traceback;


procedure Cursor
is
   package BTrees renames DB.Volatile_BTrees;

   INSERT_COUNT   : constant        := 10_000;
   FILE_NAME      : constant String := This_Computer.BTree_File;

   Stop_Now : exception;

   subtype Key_Type is DB.Types.Keys.Key_Type;
   subtype Value_Type is DB.Types.Values.Number_Type;
   subtype Timer_Type is DB.Util.Timers.Timer_Type;
   use type Key_Type;
   use type Value_Type;
   use type Timer_Type;

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


   type Procedure_Type is access procedure (Tree : in out BTrees.Tree_Type);

   procedure Execute
     (Tree        : in out BTrees.Tree_Type;
      Description : in     String;
      Proc        : in     Procedure_Type;
      Count       : in     Count_Type;
      Reset       : in     Boolean := True)
   is
      Total_Timer : Timer_Type;
   begin
      if Reset then
         Reset_String_Generation;
      end if;
      DB.Util.Timers.Start(Total_Timer);
      for I in 1 .. Count loop
         declare
         begin
            Proc(Tree);
         exception
            when Stop_Now =>
               null;
            when Error : others =>
               Put_Line("Exception: "& Exception_Message(Error));
               Put_Line("Exception: "& Exception_Information(Error));
               DB.Util.Traceback.Print_Traceback;
               DB.Util.Traceback.Print_Traceback(Error);
         end;
      end loop;
      DB.Util.Timers.Stop(Total_Timer);
      DB.Util.Timers.Print(Description & Count_Type'Image(Count), Total_Timer);
   end Execute;


   procedure Perform_Insertion (Tree  : in out BTrees.Tree_Type)
   is
      use type BTrees.Count_Type;
      use type BTrees.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Pos    : BTrees.Count_Type;
      State  : BTrees.Result_Type;
   begin
      BTrees.Insert(Tree, KV.Key, KV.Value, Pos, State);
      if State /= BTrees.Success then
         Put_Line("Insertion failed");
      end if;
   end Perform_Insertion;


   procedure Perform_Deletion (Tree  : in out BTrees.Tree_Type)
   is
      use type BTrees.Count_Type;
      use type BTrees.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Val    : Value_Type;
      Pos    : BTrees.Count_Type;
      State  : BTrees.Result_Type;
   begin
      BTrees.Delete(Tree, KV.Key, Val, Pos, State);
      if State /= BTrees.Success or else KV.Value /= Val then
         Put_Line("Deletion failed");
      end if;
   end Perform_Deletion;


   procedure Perform_Search (Tree  : in out BTrees.Tree_Type)
   is
      use type BTrees.Count_Type;
      use type BTrees.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Val    : Value_Type;
      Pos    : BTrees.Count_Type;
      State  : BTrees.Result_Type;
   begin
      BTrees.Look_Up(Tree, KV.Key, Val, Pos, State);
      if State /= BTrees.Success or else KV.Value /= Val then
         Put_Line("Look up failed "& BTrees.Result_Type'Image(State));
      end if;
   end Perform_Search;


   procedure Perform_Antisearch (Tree  : in out BTrees.Tree_Type)
   is
      use type BTrees.Count_Type;
      use type BTrees.Result_Type;
      use type DB.IO.Blocks.Size_Type;
      KV     : constant Key_Value_Type := Random_Entry;
      Val    : Value_Type;
      Pos    : BTrees.Count_Type;
      State  : BTrees.Result_Type;
   begin
      BTrees.Look_Up(Tree, KV.Key, Val, Pos, State);
      if State /= BTrees.Failure then
         Put_Line("Look up failed");
      end if;
   end Perform_Antisearch;

   procedure Iterate (Tree              : in out BTrees.Tree_Type;
                      Thread_Safe       : in     Boolean;
                      Lower             : in     BTrees.Bound_Type;
                      Upper             : in     BTrees.Bound_Type;
                      Reverse_Direction : in     Boolean;
                      Deletions         : in     Natural;
                      Reinsertions      : in     Natural)
   is
      Trans     : BTrees.RO_Transaction_Type := BTrees.New_RO_Transaction(Tree);
      Cursor    : BTrees.Cursor_Type := BTrees.New_Cursor(Tree, Trans, 
                                                          Thread_Safe, Lower, 
                                                          Upper,
                                                          Reverse_Direction);
      Prev_Key  : Key_Type;
      Key       : Key_Type;
      Value     : Value_Type;
      Pos       : BTrees.Count_Type;
      State     : BTrees.Result_Type;
      I         : Natural := 0;
   begin
      BTrees.Start_Transaction(Tree, Trans);
      loop
         I := I + 1;

         BTrees.Next(Tree, Trans, Cursor, Key, Value, State);
         case State is
            when BTrees.Success =>
               null;--Put_Line("N   Success"& Natural'Image(I));
               --Put_Line("Key: "& To_Strings.To_String(Key));
            when BTrees.Failure =>
               Put_Line("N   Failure"& Natural'Image(I));
               BTrees.Finalize(Tree, Cursor);
               return;
            when BTrees.Error =>
               Put_Line("N   Error"& Natural'Image(I));
               BTrees.Finalize(Tree, Cursor);
               return;
         end case;

         if I > 1 and then ((not Reverse_Direction and then Key <= Prev_Key)
                   or else (Reverse_Direction and then Prev_Key <= Key)) then
            Put_Line("Key error: "& To_Strings.To_String(Prev_Key) &
                     " and "& To_Strings.To_String(Key));
            BTrees.Finalize(Tree, Cursor);
            return;
         end if;
         Prev_Key := Key;

         -- Delete the element using Cursor's deletion.
         if Deletions /= 0 and then I mod Deletions = 0 then
            BTrees.Delete(Tree, Trans, Cursor, State);
            case State is
               when BTrees.Success =>
                  null;--Put_Line("D   Success"& Natural'Image(I));
               when BTrees.Failure =>
                  Put_Line("D   Failure"& Natural'Image(I));
                  BTrees.Finalize(Tree, Cursor);
                  return;
               when BTrees.Error =>
                  Put_Line("D   Error"& Natural'Image(I));
                  BTrees.Finalize(Tree, Cursor);
                  return;
            end case;

            -- Reinsert deleted element and check whether it's there by 
            -- looking it up. Includes Pause and Unpause.
            if Reinsertions /= 0 and then I mod Reinsertions = 0 then
               BTrees.Pause(Tree, Cursor);
               BTrees.Insert(Tree, Key, Value, Pos, State);
               case State is
                  when BTrees.Success =>
                     null;--Put_Line("I   Success"& Natural'Image(I));
                  when BTrees.Failure =>
                     Put_Line("I   Failure"& Natural'Image(I));
                     BTrees.Finalize(Tree, Cursor);
                     return;
                  when BTrees.Error =>
                     Put_Line("I   Error"& Natural'Image(I));
                     BTrees.Finalize(Tree, Cursor);
                     return;
               end case;
               BTrees.Unpause(Tree, Trans, Cursor);

               BTrees.Look_Up(Tree, Trans, Key, Value, Pos, State);
               case State is
                  when BTrees.Success =>
                     null;--Put_Line("S   Success"& Natural'Image(I));
                  when BTrees.Failure =>
                     Put_Line("S   Failure"& Natural'Image(I));
                     BTrees.Finalize(Tree, Cursor);
                     return;
                  when BTrees.Error =>
                     Put_Line("S   Error"& Natural'Image(I));
                     BTrees.Finalize(Tree, Cursor);
                     return;
               end case;
            end if;
         end if;
      end loop;
      BTrees.Finish_Transaction(Tree, Trans);
   exception
      when others =>
         BTrees.Finish_Transaction(Tree, Trans);
   end Iterate;

   function S(Str : String) return Key_Type
   is begin
      return (Row    => DB.Types.Rows.New_String
                        (DB.Types.Rows.Indefinite_Buffer_Type(Str)),
              Column => DB.Types.Columns.New_String
                        (DB.Types.Columns.Indefinite_Buffer_Type(Str)),
              Time   => DB.Types.Times.Number_Type'(0));
   end S;

   procedure Iterate (Tree          : in out BTrees.Tree_Type;
                      Deletions     : Natural := 0;
                      Reinnsertions : Natural := 0)
   is
      L  : constant String := "A";
      LC : constant BTrees.Comparison_Type := BTrees.Greater_Or_Equal;
      LB : constant BTrees.Bound_Type := BTrees.New_Bound(LC, S(L));
--    LB : constant BTrees.Bound_Type := BTrees.Negative_Infinity_Bound;

      U  : constant String := "K";
      UC : constant BTrees.Comparison_Type := BTrees.Less_Or_Equal;
      UB : constant BTrees.Bound_Type := BTrees.New_Bound(UC, S(U));
--    UB : constant BTrees.Bound_Type := BTrees.Positive_Infinity_Bound;
   begin
      Put_Line("Iterating");
      Iterate(Tree, True, LB, UB, True, Deletions, Reinnsertions);
      Put_Line("Done");
   end Iterate;

   use type BTrees.Result_Type;
   Tree : BTrees.Tree_Type;
   Cnt  : BTrees.Count_Type;
begin
   declare
   begin
      BTrees.Create(FILE_NAME);
      Put_Line("Newly created BTree "& FILE_NAME);
   exception
      when DB.IO.IO_Error => Put_Line("Using existing BTree "& FILE_NAME);
   end;
   BTrees.Initialize(Tree, FILE_NAME);
   BTrees.Count(Tree, Cnt);

   Init_Key_Value_Pairs(Count_Type(Cnt)*10+1);
   Put_Line("Size ="& BTrees.Count_Type'Image(Cnt));
   Put_Line("Init ="& Count_Type'Image(Count_Type(Cnt)*10+1));

   Execute(Tree, "INSERTING", Perform_Insertion'Access, INSERT_COUNT);
   Check(Tree);
   Make_Stats(Tree);

   Execute(Tree, "SEARCH", Perform_Search'Access, INSERT_COUNT);
   Check(Tree);
   Make_Stats(Tree);

   declare
      task Iteration_Task_A;
      task body Iteration_Task_A
      is begin
         Iterate(Tree, 10, 0);
         Put_Line("Task 1 finished");
      end Iteration_Task_A;
      task Iteration_Task_B;
      task body Iteration_Task_B
      is begin
         Iterate(Tree, 0, 0);
         Put_Line("Task 2 finished");
      end Iteration_Task_B;
   begin
      null;
   end;
   Iterate(Tree, 0, 0);

   Make_Stats(Tree);
   BTrees.Finalize(Tree);

exception
   when Stop_Now =>
      null;
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Util.Traceback.Print_Traceback;
      Put_Line("Traceback");
      DB.Util.Traceback.Print_Traceback(Error);
      Put_Line("Traceback");

end Cursor;

