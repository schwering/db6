with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Text_IO;

package body REST.Error_Log is

   function Now return String is
      package C renames Ada.Calendar;
      package CF renames Ada.Calendar.Formatting;

      function Strip (S : String) return String is
      begin
         if S (S'First) = ' ' then
            return S (S'First + 1 .. S'Last);
         else
            return S;
         end if;
      end Strip;

      Now     : constant C.Time := C.Clock;
      Now_Str : constant String :=
         Strip (C.Year_Number'Image (CF.Year (Now))) & "-" &
         Strip (C.Month_Number'Image (CF.Month (Now))) & "-" &
         Strip (C.Day_Number'Image (CF.Day (Now))) & " " &
         Strip (CF.Hour_Number'Image (CF.Hour (Now))) & ":" &
         Strip (CF.Minute_Number'Image (CF.Minute (Now))) & ":" &
         Strip (CF.Second_Number'Image (CF.Second (Now)));
   begin
      return Now_Str;
   end Now;


   procedure Log_To_Stdout (Msg : in String) is
   begin
      Ada.Text_IO.Put_Line ("Error caught ("& Now &"):");
      Ada.Text_IO.Put_Line (Msg);
      Ada.Text_IO.New_Line;
   end Log_To_Stdout;


   procedure Log_To_Stdout (Exc : in Ada.Exceptions.Exception_Occurrence) is
   begin
      Log (Ada.Exceptions.Exception_Information (Exc));
   end Log_To_Stdout;


   procedure Log (Msg : in String)
   renames Log_To_Stdout;

   procedure Log (Exc : in Ada.Exceptions.Exception_Occurrence)
   renames Log_To_Stdout;

end REST.Error_Log;

