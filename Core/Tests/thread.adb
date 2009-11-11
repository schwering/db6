-- vim:tabstop=3:softtabstop=3:shiftwidth=3:expandtab

with Ada.Text_IO; use Ada.Text_IO;

with DB.Compression.Gen_Levenshtein;
with DB.Utils.Timers;

procedure Thread
is

   -- Uses the CPU heavily. (Compresses and decompresses `Count' times 
   -- two strings using Levenshtein delta encoding.)
   procedure Work (Count : in Natural)
   is
      procedure Test (S, T : in String)
      is
         package Levenshtein is new DB.Compression.Gen_Levenshtein
           (Item_Type             => Character,
            Item_Array_Type       => String,
            Max_Length            => 255);
         Compression_Error : exception;
         D : constant Levenshtein.Delta_Type := Levenshtein.Encode(S, T);
         U : constant String                 := Levenshtein.Decode(S, D);
      begin
         if T /= U then
            raise Compression_Error;
         end if;
      end Test;

      S1 : constant String := "Eine interessante Eigenschaft von OpenMP ist, "&
                              "dass(bis auf Ausnahmen) die Programme";
      S2 : constant String := "Threaderzeugung: omp parallel teilt das "&
                              "Programm Originalthread in mehrere Threads auf";
   begin
      for I in 1 .. Count loop
         Test(S1, S2);
         Test(S2, S1);
      end loop;
   end Work;



   Count     : constant Natural := 10;
   Work_Size : constant Natural := 10_000;
   T : DB.Utils.Timers.Timer_Type;
begin
   DB.Utils.Timers.Reset(T);
   DB.Utils.Timers.Start(T);

   declare
      procedure Do_Not is
      -- Both, man and woman do work `Count' times.
      -- After each time, they have a rendez-vous.
      task Man is
         entry Meet (J : in Integer);
      end Man;
      task Woman;

      task body Man is
      begin
         for I in 1 .. Count loop
            Work(Work_Size);
            declare
               T : DB.Utils.Timers.Timer_Type;
            begin
               DB.Utils.Timers.Start(T);
               select
                  accept Meet (J : in Integer) do
                     Put_Line("Love is in the air!"& 
                        Integer'Image(I) &" ="& Integer'Image(J));
                  end Meet;
               end select;
               DB.Utils.Timers.Stop(T);
               DB.Utils.Timers.Print("Man waited for woman", T);
            end;
         end loop;
         Put_Line("Man is done.");
      end Man;

      task body Woman is
      begin
         for I In 1 .. Count loop
            Work(Work_Size);
            declare
               T : DB.Utils.Timers.Timer_Type;
            begin
               DB.Utils.Timers.Start(T);
               Man.Meet(I);
               DB.Utils.Timers.Stop(T);
               DB.Utils.Timers.Print("Woman waited for man", T);
            end;
         end loop;
         Put_Line("Woman is done.");
      end Woman;
      begin null; end Do_Not;

      pragma Unreferenced (Do_Not);

      task type Worker;

      task body Worker is
      begin
         Put_Line("Worker starts working");
         for I in 1 .. Count loop
            Work(Work_Size);
         end loop;
         Put_Line("Worker done");
      end Worker;

      Workers : array (1 .. 4) of Worker;
      pragma Unreferenced (Workers);
   begin
      null;
   end;

   DB.Utils.Timers.Stop(T);
   DB.Utils.Timers.Print("Finished", T);
end Thread;

