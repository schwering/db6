with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with DB.Utils.Timers;

package body IO_Dispatcher.Jobs is

   function To_Description (S : String) return Description_Type
   is
      D : Description_Type;
   begin
      D.Len := S'Length;
      D.Str(1 .. S'Length) := S;
      return D;
   end To_Description;


   function To_String (D : Description_Type) return String is
   begin
      return D.Str(1 .. D.Len);
   end To_String;


   function "=" (D, E : Description_Type) return Boolean is
   begin
      return D.Len = E.Len and then D.Str(1 .. D.Len) = E.Str(1 .. E.Len);
   end "=";


   function Add
     (Job_Map     : Map_Type;
      Description : String;
      Short_Job   : Short_Job_Type)
      return Map_Type
   is
      New_Job_Map : Map_Type(Job_Map'First .. Job_Map'Last + 1);
   begin
      New_Job_Map(Job_Map'Range)    := Job_Map;
      New_Job_Map(Job_Map'Last + 1) := (To_Description(Description), Short_Job);
      return New_Job_Map;
   end Add;


   function New_Job
     (Description               : in Description_Type;
      Short_Job                 : in Short_Job_Type;
      Reset                     : in Boolean := True)
      return Job_Type is
   begin
      return New_Job(Description, Short_Job, 1, 1, Reset);
   end New_Job;


   function New_Job
     (Description               : in Description_Type;
      Short_Job                 : in Short_Job_Type;
      Short_Job_Execution_Count : in Random.Count_Type;
      Concurrency_Degree        : in Positive;
      Reset                     : in Boolean)
      return Job_Type is
   begin
      return (Description, Short_Job, Short_Job_Execution_Count,
              Concurrency_Degree, Reset);
   end New_Job;


   procedure Execute_Jobs
     (Jobs : in Long_Job_Type) is
   begin
      for I in Jobs'Range loop
         Execute_Job(Jobs(I));
      end loop;
   end Execute_Jobs;


   procedure Execute_Job
     (Job : in Job_Type) is
   begin
      Execute_Job(Job.Description, Job.Short_Job, Job.Short_Job_Execution_Count,
                  Job.Concurrency_Degree, Job.Reset);
   end Execute_Job;


   procedure Execute_Job
     (Description               : in Description_Type;
      Short_Job                 : in Short_Job_Type;
      Short_Job_Execution_Count : in Random.Count_Type;
      Concurrency_Degree        : in Positive;
      Reset                     : in Boolean)
   is
      use type Random.Count_Type;
      Total_Timer : DB.Utils.Timers.Timer_Type;
   begin
      if Reset then
         Random.Reset_String_Generation;
      end if;
      DB.Utils.Timers.Start(Total_Timer);
      declare
         task type Task_Type is
            entry Set_Loop_Count (Count : in Random.Count_Type);
         end;

         task body Task_Type
         is
            N : Random.Count_Type;
         begin
            accept Set_Loop_Count (Count : in Random.Count_Type) do
               N := Count;
            end Set_Loop_Count;
            for I in 1 .. N loop
               declare
               begin
                  Short_Job.all;
               exception
                  when Error : others =>
                     Put_Line("Exception: "& Exception_Information(Error));
                     declare
                        I : access Integer := null;
                        J : Integer;
                        procedure Kill_Program (Exit_Value : in Integer);
                        pragma Import (C, Kill_Program, "exit");
                     begin
                        J := I.all;
                        Kill_Program(1);
                     end;
                     exit;
               end;
            end loop;
         exception
            when Error : others =>
               Put_Line("Exception: "& Exception_Information(Error));
         end Task_Type;

         Tasks : array (1 .. Concurrency_Degree) of Task_Type;
         J : constant Random.Count_Type := Short_Job_Execution_Count;
         T : constant Random.Count_Type
           := Random.Count_Type(Concurrency_Degree);
         N : constant Random.Count_Type := J / T;
         R :          Random.Count_Type := J mod T;
      begin
         for I in Tasks'Range loop
            if R > 0 then
               Tasks(I).Set_Loop_Count(N + 1);
               R := R - 1;
            else
               Tasks(I).Set_Loop_Count(N);
            end if;
         end loop;
      end;
      DB.Utils.Timers.Stop(Total_Timer);
      DB.Utils.Timers.Print(To_String(Description) &
                           Random.Count_Type'Image(Short_Job_Execution_Count) &
                           Positive'Image(Concurrency_Degree) & " " &
                           Boolean'Image(Reset), Total_Timer);
   end Execute_Job;

end IO_Dispatcher.Jobs;

