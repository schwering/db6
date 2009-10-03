-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Random; use Random;
with This_Computer;
with To_Strings;

with DB.IO.Blocks;
with DB.IO.Blocks.Direct_IO;

with DB.Gen_Blob_Trees;

with DB.Types;
with DB.Types.Rows;
with DB.Types.Columns;
with DB.Types.Times;
with DB.Types.Keys;
with DB.Types.Values;

with DB.Util.Timers;
with DB.Util.Traceback;

with System.Storage_Elements;

procedure Blob_Cursor
is
   subtype Key_Type is DB.Types.Keys.Key_Type;
   subtype Value_Type is DB.Types.Values.Value_Type;
   subtype Timer_Type is DB.Util.Timers.Timer_Type;
   use type Key_Type;
   use type Value_Type;
   use type Timer_Type;

   function To_Storage_Array
     (Value : Value_Type)
      return System.Storage_Elements.Storage_Array
   is
      Len    : constant DB.IO.Blocks.Position_Type
             := DB.IO.Blocks.Position_Type
                   (DB.IO.Blocks.Bits_To_Units(Value'Size));
      Cursor : DB.IO.Blocks.Cursor_Type;
      Block  : DB.IO.Blocks.Base_Block_Type(1 .. Len);
      procedure Write is new DB.IO.Blocks.Write(Value_Type);
   begin
      Write(Block, Cursor, Value);
      return System.Storage_Elements.Storage_Array(Block);
   end;

   function From_Storage_Array
     (Block : System.Storage_Elements.Storage_Array)
      return Value_Type
   is
      procedure Read is new DB.IO.Blocks.Read(Value_Type);
      Cursor : DB.IO.Blocks.Cursor_Type;
      Value  : Value_Type;
   begin
      Read(DB.IO.Blocks.Base_Block_Type(Block), Cursor, Value);
      return Value;
   end;

   package BTrees is new DB.Gen_Blob_Trees
     (Key_Type                      => Key_Type,
      Key_Context_Type              => DB.Types.Keys.Context_Type,
      Write_Key                     => DB.Types.Keys.Write,
      Read_Key                      => DB.Types.Keys.Read,
      Skip_Key                      => DB.Types.Keys.Skip,
      "="                           => DB.Types.Keys."=",
      "<="                          => DB.Types.Keys."<=",
      Value_Type                    => Value_Type,
      To_Storage_Array              => To_Storage_Array,
      From_Storage_Array            => From_Storage_Array,
      Is_Context_Free_Serialization =>
                     DB.Types.Keys.Is_Context_Free_Serialization,
      Block_IO                      => DB.IO.Blocks.Direct_IO.IO);

   INSERT_COUNT   : constant        := 10_000;
   FILE_NAME      : constant String := ".tmp/blog";--This_Computer.BTree_File;


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
      Trans     : BTrees.RW_Transaction_Type := BTrees.New_RW_Transaction(Tree);
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
               exit;
            when BTrees.Error =>
               Put_Line("N   Error"& Natural'Image(I));
               exit;
         end case;

         if I > 1 and then ((not Reverse_Direction and then Key <= Prev_Key)
                   or else (Reverse_Direction and then Prev_Key <= Key)) then
            Put_Line("Key error: "& To_Strings.To_String(Prev_Key) &
                     " and "& To_Strings.To_String(Key));
            exit;
         end if;
         Prev_Key := Key;

         -- Delete the element using Cursor's deletion.
         if Deletions /= 0 and then I mod Deletions = 0 then
            BTrees.Delete(Tree, Trans, Cursor, Key, Value, Pos, State);
            case State is
               when BTrees.Success =>
                  null;--Put_Line("D   Success"& Natural'Image(I));
               when BTrees.Failure =>
                  Put_Line("D   Failure"& Natural'Image(I));
                  exit;
               when BTrees.Error =>
                  Put_Line("D   Error"& Natural'Image(I));
                  exit;
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
                     exit;
                  when BTrees.Error =>
                     Put_Line("I   Error"& Natural'Image(I));
                     exit;
               end case;

               BTrees.Look_Up(Tree, Trans, Key, Value, Pos, State);
               case State is
                  when BTrees.Success =>
                     null;--Put_Line("S   Success"& Natural'Image(I));
                  when BTrees.Failure =>
                     Put_Line("S   Failure"& Natural'Image(I));
                     exit;
                  when BTrees.Error =>
                     Put_Line("S   Error"& Natural'Image(I));
                     exit;
               end case;
               BTrees.Unpause(Tree, Trans, Cursor);
            end if;
         end if;
      end loop;
      BTrees.Finalize(Tree, Cursor);
      BTrees.Commit_Transaction(Tree, Trans);
   exception
      when others =>
         BTrees.Finalize(Tree, Cursor);
         BTrees.Commit_Transaction(Tree, Trans);
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
   Execute(Tree, "SEARCH", Perform_Search'Access, INSERT_COUNT);

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

   BTrees.Finalize(Tree);

exception
   when Error : others =>
      Put_Line("Exception: "& Exception_Message(Error));
      Put_Line("Exception: "& Exception_Information(Error));
      DB.Util.Traceback.Print_Traceback;
      Put_Line("Traceback");
      DB.Util.Traceback.Print_Traceback(Error);
      Put_Line("Traceback");

end Blob_Cursor;

