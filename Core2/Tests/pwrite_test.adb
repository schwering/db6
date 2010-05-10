with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with DB.Blocks;
with DB.Blocks.Local_IO;

procedure PWrite_Test
is
   package Blocks renames DB.Blocks;
   package IO renames DB.Blocks.Local_IO;

   Err : exception;

   Block_Count : constant := 3;
   Writer_Count : constant := 1;
   Reader_Count  : constant := 1;

   function Rand return Integer;
   pragma Import (C, Rand, "rand");

   function New_Block return Blocks.Block_Type
   is
      use type Blocks.Base_Position_Type;
      procedure Write is new Blocks.Write_At(Integer);
      I     : constant Integer := Rand;
      Block : Blocks.Block_Type;
   begin
      Write(Block, 1, 4, I);
      Write(Block, 4092 + 1, 4096 + 4, I);
      return Block;
   end New_Block;

   procedure Check_Block (Block : Blocks.Block_Type)
   is
      use type Blocks.Base_Position_Type;
      procedure Read is new Blocks.Read_At(Integer);
      I, J : Integer;
   begin
      Read(Block, 1, 4, I);
      Read(Block, 4092 + 1, 4096 + 4, J);
      if I /= J then
         Put_Line("I /= J:"& I'Img &" /="& J'Img);
         raise Constraint_Error;
      else
         Put_Line("I = J:"& I'Img &" ="& J'Img);
      end if;
   end Check_Block;

   F   : IO.File_Type;
   Max : IO.Valid_Address_Type;

   function Rand_Addr return IO.Valid_Address_Type
   is
      use type IO.Valid_Address_Type;
      R : constant Integer := Rand;
   begin
      return IO.Valid_Address_Type(R) mod Max + 1;
   end Rand_Addr;

begin
   IO.Create_And_Open_Temporary(".tmp/pwrite_test", F);

   for I in 1 .. Block_Count loop
      IO.Write_New_Block(F, Max, New_Block);
   end loop;

   declare
      task type Writer;
      task body Writer
      is
         A : IO.Valid_Address_Type;
      begin
         loop
            --if Rand mod 100000000 = 0 then
               --A := Max;
               --IO.Write_New_Block(F, A, New_Block);
            --else
               A := Rand_Addr;
               IO.Write(F, A, New_Block);
            --end if;
            Put_Line("Wrote at"& A'Img);
         end loop;
      exception
         when Error : others =>
            Put_Line("Exception: "& Exception_Information(Error));
      end Writer;

      task type Reader;
      task body Reader
      is
         Block : Blocks.Block_Type;
         A     : IO.Valid_Address_Type;
      begin
         loop
            A := Rand_Addr;
            IO.Read(F, A, Block);
            Put_Line("Read at"& A'Img);
            Check_Block(Block);
         end loop;
      exception
         when Error : others =>
            Put_Line("Exception: "& Exception_Information(Error));
      end Reader;

      Arr  : array (1 .. Writer_Count) of Writer;
      Arr2 : array (1 .. Reader_Count) of Reader;
   begin
      null;
   end;
   IO.Close(F);
end PWrite_Test;

