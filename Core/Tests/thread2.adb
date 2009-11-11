-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Random; use Random;
with To_Strings;
with This_Computer;

with DB.Volatile_BTrees;
with DB.Gen_BTrees.Gen_Check;

with DB.Types.Values;

with DB.Utils.Timers;

procedure Thread2
is
   --package BTree renames DB.BTree;
   package BTrees renames DB.Volatile_BTrees;

   FILE_NAME : constant String := This_Computer.BTree_File;

   Tree  : BTrees.Tree_Type;


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
         raise;
   end Check;


   procedure Searches (S     : String;
                       Count : BTrees.Count_Type)
   is
      pragma Unreferenced (S);
      use type BTrees.Count_Type;
      use type BTrees.Result_Type;
   begin
      for I in 1 .. Count/2 loop
         declare
            KV    : constant Key_Value_Type := Random_Entry;
            Val   : DB.Types.Values.Value_Type;
            Pos   : BTrees.Count_Type;
            State : BTrees.Result_Type;
         begin
            BTrees.Look_Up(Tree, KV.Key, Val, Pos, State);
            --if State /= BTrees.Success then
               --Put_Line(S &" "& BTrees.Result_Type'Image(State));
            --end if;
         end;
      end loop;
   end Searches;


   procedure Insertions (S     : String;
                         Count : BTrees.Count_Type)
   is
      use type BTrees.Result_Type;
      use type BTrees.Count_Type;
   begin
      for I in 1 .. Count loop
         declare
            KV    : constant Key_Value_Type := Random_Entry;
            Pos   : BTrees.Count_Type;
            State : BTrees.Result_Type;
         begin
            BTrees.Insert(Tree, KV.Key, KV.Value, Pos, State);
            Check(Tree);
            if State /= BTrees.Success then
               Put_Line(S &" "& BTrees.Result_Type'Image(State));
            end if;
         exception
            when Error : others =>
               Put_Line("Exception: "& Exception_Message(Error));
               Put_Line("Exception: "& Exception_Information(Error));
         end;
      end loop;
   end Insertions;


   type Procedure_Access_Type is
      access procedure (S     : String;
                        Count : BTrees.Count_Type);

   procedure Perform (Proc : Procedure_Access_Type;
                      Init : Boolean := False;
                      Cnt  : BTrees.Count_Type := 0)
   is
      use type BTrees.Count_Type;
      T : DB.Utils.Timers.Timer_Type;
      Current_Count : BTrees.Count_Type;
      Count         : BTrees.Count_Type := Cnt;
   begin
      Check(Tree);
      Put_Line("Check: OK");
      BTrees.Count(Tree, Current_Count);
      Put_Line("Count:"& BTrees.Count_Type'Image(Current_Count));
      if Init then
         Init_Key_Value_Pairs(Count_Type(Current_Count) + 1);
      else
         Init_Key_Value_Pairs(1);
      end if;
      if Count = 0 then
         Count := Current_Count;
      end if;
      Reset_String_Generation;

      DB.Utils.Timers.Reset(T);
      DB.Utils.Timers.Start(T);
      declare
         task Man;
         task body Man is
         begin
            Put_Line("Man starts.");
            Proc("Man", Count);
            Put_Line("Man is done.");
         end Man; -- begin null; end; declare -- sequential or parallel

         task Woman;
         task body Woman is
         begin
            Put_Line("Woman starts.");
            Proc("Woman", Count);
            Put_Line("Woman is done.");
         end Woman; -- begin null; end; declare -- sequential or parallel

         task Lukas;
         task body Lukas is
         begin
            Put_Line("Lukas starts.");
            Searches("Lukas", Count);
            Searches("Lukas", Count);
            Put_Line("Lukas is done.");
         end Lukas;
      begin
         null;
      end;
      DB.Utils.Timers.Stop(T);
      DB.Utils.Timers.Print("Finished", T);
   end Perform;

begin
   declare
   begin
      BTrees.Create(FILE_NAME);
   exception
      when others => null;
   end;

   BTrees.Initialize(Tree, FILE_NAME);

   Put_Line("INSERTIONS");
   Perform(Insertions'Access, True, 100_000);
   Put_Line("SEARCHING");
   Perform(Searches'Access, False, 0);

end Thread2;

