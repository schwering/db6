with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

package body IO_Dispatcher.Args is

   function To_Number (S : String) return Long_Long_Integer
   is
      J : Long_Long_Integer := 0;
   begin
      for I in S'Range loop
         if Character'Pos(S(I)) in
               Character'Pos('0') ..  Character'Pos('9') then
            J := J * 10 + (Character'Pos(S(I)) - Character'Pos('0'));
         elsif S(I) = 'k' then
            J := J * 1000;
         elsif S(I) = 'm' then
            J := J * 1000 * 1000;
         end if;
      end loop;
      pragma Assert (J'Valid);
      return J;
   end To_Number;


   Offset : Natural := 0;
   -- Parameters can be processed in which case they `disappear'.
   -- To realize this, an offset is always added.


   function Pop_Argument (I : Positive) return String is
      Arg : constant String := Ada.Command_Line.Argument(Offset + I);
   begin
      Offset := Offset + 1;
      return Arg;
   end Pop_Argument;


   function File_Name return String is
   begin
      return Ada.Command_Line.Argument(Offset + 1);
   end File_Name;


   procedure Undo_Pop is
   begin
      Offset := Offset - 1;
   end Undo_Pop;


   function Init_Offset return Random.Count_Type is
   begin
      return Random.Count_Type(To_Number(Ada.Command_Line.Argument(Offset+2)));
   end Init_Offset;


   function Create_Jobs_From_Command_Line
     (Map : Jobs.Map_Type)
      return Jobs.Long_Job_Type
   is
      function New_Job (S : String) return Jobs.Job_Type
      is
         function Retrieve (D : Jobs.Description_Type) return Jobs.Short_Job_Type
         is
            use type Jobs.Description_Type;
         begin
            for I in Map'Range loop
               if Map(I).Description = D then
                  return Map(I).Short_Job;
               end if;
            end loop;
            Put_Line("Couldn't look up "& Jobs.To_String(D));
            raise Parse_Error;
         end Retrieve;

         From                      : Natural := S'First;
         To                        : Natural := 0;
         Description               : Jobs.Description_Type;
         Short_Job                 : Jobs.Short_Job_Type;
         Short_Job_Execution_Count : Random.Count_Type;
         Concurrency_Degree        : Positive;
         Reset                     : Boolean;
      begin
         for I in From .. S'Last loop
            if S(I) = ',' then
               To := I;
               exit;
            end if;
         end loop;
         if To = 0 then
            Description := Jobs.To_Description(S);
            return Jobs.New_Job(Description, Retrieve(Description), 1, 1, True);
         end if;

         Description := Jobs.To_Description(S(From .. To - 1));
         Short_Job   := Retrieve(Description);

         From := To + 1;
         To   := 0;
         for I in From .. S'Last loop
            if S(I) = ',' then
               To := I;
               exit;
            end if;
         end loop;
         Concurrency_Degree := 10;
         if To = 0 then
            Short_Job_Execution_Count := Random.Count_Type(To_Number
                                             (S(From .. S'Last)));
            Reset                     := True;
         else
            Short_Job_Execution_Count := Random.Count_Type(To_Number
                                             (S(From .. To - 1)));

            --From := To + 1;
            --To   := 0;
            --for I in From .. S'Last loop
               --if S(I) = ',' then
                  --To := I;
                  --exit;
               --end if;
            --end loop;
            --Concurrency_Degree := Positive(To_Number(S(From .. To - 1)));

            From := To + 1;
            if S(From .. S'Last) = "Reset" then
               Reset := True;
            elsif S(From .. S'Last) = "Cont" then
               Reset := False;
            else
               raise Parse_Error;
            end if;

         end if;

         return Jobs.New_Job(Description, Short_Job, Short_Job_Execution_Count,
                             Concurrency_Degree, Reset);
      end New_Job;

      Long_Job : Jobs.Long_Job_Type(Offset+3..Ada.Command_Line.Argument_Count);
   begin
      for I in Long_Job'Range loop
         Long_Job(I) := New_Job(Ada.Command_Line.Argument(I));
      end loop;
      return Long_Job;
   end Create_Jobs_From_Command_Line;

end IO_Dispatcher.Args;

