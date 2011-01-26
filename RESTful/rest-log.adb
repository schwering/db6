-- Abstract:
--
-- see spec
--
-- Copyright 2010--2011 Christoph Schwering

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Text_IO;

package body REST.Log is

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


   function Img (N : Integer) return String
   is
      S : constant String := Integer'Image (N);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Img;


   procedure Log_To_Stdout
     (Msg    : in String;
      Source : in String := GNAT.Source_Info.File;
      Line   : in Natural := GNAT.Source_Info.Line) is
   begin
      Ada.Text_IO.Put (Source &":"& Img (Line) &": ");
      Ada.Text_IO.Put (Kind &" ("& Now &"): ");
      --Ada.Text_IO.New_Line;
      Ada.Text_IO.Put (Msg);
      Ada.Text_IO.New_Line;
   end Log_To_Stdout;


   procedure Info
     (Msg    : in String;
      Source : in String := GNAT.Source_Info.File;
      Line   : in Natural := GNAT.Source_Info.Line)
   is
      procedure Log is new Log_To_Stdout ("Info");
   begin
      Log (Msg, Source, Line);
   end Info;


   procedure Error
     (Msg    : in String;
      Source : in String := GNAT.Source_Info.File;
      Line   : in Natural := GNAT.Source_Info.Line)
   is
      procedure Log is new Log_To_Stdout ("Error");
   begin
      Log (Msg, Source, Line);
   end Error;


   procedure Error
     (Exc    : in Ada.Exceptions.Exception_Occurrence;
      Source : in String := GNAT.Source_Info.File;
      Line   : in Natural := GNAT.Source_Info.Line)
   is
      procedure Log is new Log_To_Stdout ("Exception");
   begin
      Log (Ada.Exceptions.Exception_Information (Exc), Source, Line);
   end Error;

end REST.Log;

