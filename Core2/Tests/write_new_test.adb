with Ada.Text_IO; use Ada.Text_IO;
with DB.Blocks;
with DB.Blocks.Local_IO;

procedure Write_New_Test
is
   package Blocks renames DB.Blocks;
   package IO renames DB.Blocks.Local_IO;

   Err : exception;

   Write_Count : constant := 1000;
   Task_Count  : constant := 100;

   type Buffer_Type is array (1 .. Write_Count * Task_Count) of IO.Valid_Address_Type;

   protected Buffer is
      procedure Put (A : IO.Valid_Address_Type);
      function Size return Natural;
      function Get (I : Positive) return IO.Valid_Address_Type;
   private
      Buf : Buffer_Type;
      S   : Natural := 0;
   end Buffer;

   protected body Buffer is
      procedure Put (A : IO.Valid_Address_Type) is
      begin
         S := S + 1;
         Buf(S) := A;
      end Put;

      function Size return Natural is
      begin
         return S;
      end Size;

      function Get (I : Positive) return IO.Valid_Address_Type is
      begin
         return Buf(I);
      end Get;
   end Buffer;

   F : IO.File_Type;
begin
   IO.Create_And_Open_Temporary(".tmp/write_new_test", F);
   declare
      task type Writer;
      task body Writer is
      begin
         for I in 1 .. Write_Count loop
            declare
               use type Blocks.Block_Type;
               procedure Write_Integer is new Blocks.Write(Integer);
               A  : IO.Valid_Address_Type;
               B1 : Blocks.Block_Type;
               B2 : Blocks.Block_Type;
               C  : Blocks.Cursor_Type := Blocks.New_Cursor(B1'First);
            begin
               Write_Integer(B1, C, I);
               IO.Write_New_Block(F, A, B1);
               Buffer.Put(A);
               --IO.Read(F, A, B2);
               --if B1 /= B2 then raise Err; end if;
            end;
         end loop;
      end Writer;

      Arr : array (1 .. Task_Count) of Writer;
   begin
      null;
   end;
   for I in 1 .. Buffer.Size loop
      for J in I+1 .. Buffer.Size loop
         declare
            use type IO.Valid_Address_Type;
         begin
            if Buffer.Get(I) = Buffer.Get(J) then
               raise Err;
            end if;
         end;
      end loop;
      Put_Line(I'Img &" /"& Integer'Image(Buffer.Size));
   end loop;
   IO.Close(F);
end Write_New_Test;

