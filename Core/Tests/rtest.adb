with Ada.Text_IO; use Ada.Text_IO;
with DB.BTrees;

with Random; use Random;

procedure RTest
is
   Count      : DB.BTrees.Count_Type := 990_000_000;
   Iterations : constant := 100;
   Insertions : constant := 10_000_000;
begin
   for I in 1 .. Iterations loop
      Init_Key_Value_Pairs(Count_Type(Count)*10+1);
      Put_Line("Size ="& DB.BTrees.Count_Type'Image(Count));
      Put_Line("Init ="& Count_Type'Image(Count_Type(Count)*10+1));
      for I in 1 .. Insertions loop
         declare
            KV : constant Key_Value_Type := Random_Entry;
            pragma Unreferenced (KV);
            use type DB.BTrees.Count_Type;
         begin
            Count := Count + 1;
         end;
      end loop;
   end loop;
end RTest;

