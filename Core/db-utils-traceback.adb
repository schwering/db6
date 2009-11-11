with Ada.Text_IO;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;

package body DB.Utils.Traceback is

   function Traceback_String return String
   is
      TB : GNAT.Traceback.Tracebacks_Array(1 .. 100);
      Len : Natural;
   begin
      GNAT.Traceback.Call_Chain(TB, Len);
      return GNAT.Traceback.Symbolic.Symbolic_Traceback(TB(1..Len));
      --return "(not supported)";
   end Traceback_String;


   procedure Print_Traceback is
   begin
      Ada.Text_IO.Put_Line("Symbolic Traceback:");
      Ada.Text_IO.Put(Traceback_String);
   end Print_Traceback;


   procedure Print_Traceback (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      Ada.Text_IO.Put_Line("Symbolic Traceback of Exception:");
      Ada.Text_IO.Put_Line(GNAT.Traceback.Symbolic.Symbolic_Traceback(E));
   end Print_Traceback;

end DB.Utils.Traceback;

