-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.IO.Blocks.Gen_Buffers is

   function New_Buffer
      return Buffer_Type
   is
      B : Buffer_Type;
   begin
      B.Head                    := null;
      B.New_Address_Initialized := False;
      return B;
   end New_Buffer;


   procedure Free
     (Buffer : in out Buffer_Type)
   is
      procedure Dealloc is
         new Ada.Unchecked_Deallocation(Entry_Type, Entry_Ref_Type);
      procedure Dealloc is
         new Ada.Unchecked_Deallocation(Block_Type, Block_Ref_Type);
      E : Entry_Ref_Type := Buffer.Head;
   begin
      while E /= null loop
         declare
            F : Entry_Ref_Type := E;
         begin
            E := E.Next;
            Dealloc(F.Block);
            Dealloc(F);
         end;
      end loop;
   end Free;


   procedure Commit
     (File   : in out Block_IO.File_Type;
      Buffer : in     Buffer_Type)
   is
      E : Entry_Ref_Type := Buffer.Head;
   begin
      while E /= null loop
         if E.Changed then
            Block_IO.Write(File, E.Address, E.Block.all);
         end if;
         E := E.Next;
      end loop;
   end Commit;


   procedure Seek_New
     (File    : in out Block_IO.File_Type;
      Buffer  : in out Buffer_Type;
      Address :    out Block_IO.Valid_Address_Type) is
   begin
      if not Buffer.New_Address_Initialized then
         Buffer.New_Address_Initialized := True;
         Block_IO.Seek_New(File, Buffer.New_Address);
      else
         Buffer.New_Address := Block_IO.Succ(Buffer.New_Address);
      end if;
      Address := Buffer.New_Address;
   end Seek_New;


   function "<="
     (A, B : Block_IO.Valid_Address_Type)
      return Boolean
   is
      use Block_IO; -- use type  Valid_Address_Type not sufficent
   begin
      return not (B < A);
   end "<=";


   procedure Read
     (File    : in out Block_IO.File_Type;
      Buffer  : in out Buffer_Type;
      Address : in     Block_IO.Valid_Address_Type;
      Block   :    out Block_Type)
   is
      use Block_IO; -- use type  Valid_Address_Type not sufficent
   begin
      if Buffer.Head /= null and then not (Address < Buffer.Head.Address) then
         declare
            E : Entry_Ref_Type := Buffer.Head;
         begin
            loop
               if E.Address = Address then
                  Block := E.Block.all;
                  return;
               end if;
               exit when E.Next = null or else Address < E.Next.Address;
               E := E.Next;
            end loop;
         end;
      end if;
      Block_IO.Read(File, Address, Block);
   end Read;


   procedure Read
     (File        : in out Block_IO.File_Type;
      Buffer      : in out Buffer_Type;
      Address     : in     Block_IO.Valid_Address_Type;
      Block_Ref    :    out Block_Constant_Ref_Type)
   is
      use Block_IO; -- use type  Valid_Address_Type not sufficient
   begin
      if Buffer.Head = null or else Address < Buffer.Head.Address then
         declare
            Block     : Block_Type;
         begin
            Block_IO.Read(File, Address, Block);
            Buffer.Head := new Entry_Type'(Address => Address,
                                           Block   => new Block_Type'(Block),
                                           Next    => Buffer.Head,
                                           Changed => False);
            Block_Ref := Block_Constant_Ref_Type(Buffer.Head.Block);
         end;
      else
         declare
            E : Entry_Ref_Type := Buffer.Head;
         begin
            loop
               if E.Address = Address then
                  Block_Ref := Block_Constant_Ref_Type(E.Block);
                  return;
               end if;
               exit when E.Next = null or else Address < E.Next.Address;
               E := E.Next;
            end loop;

            declare
               Block : Block_Type;
            begin
               Block_IO.Read(File, Address, Block);
               E.Next := new Entry_Type'(Address => Address,
                                         Block   => new Block_Type'(Block),
                                         Next    => E.Next,
                                         Changed => False);
               Block_Ref := Block_Constant_Ref_Type(E.Next.Block);
            end;
         end;
      end if;
   end Read;


   procedure Write
     (File    : in out Block_IO.File_Type;
      Buffer  : in out Buffer_Type;
      Address : in     Block_IO.Valid_Address_Type;
      Block   : in     Block_Type)
   is
      pragma Unreferenced (File);
      procedure Dealloc is
         new Ada.Unchecked_Deallocation(Block_Type, Block_Ref_Type);
      use Block_IO; -- use type Valid_Address_Type not sufficent
   begin
      if Buffer.Head = null or else Address < Buffer.Head.Address then
         Buffer.Head := new Entry_Type'(Address => Address,
                                        Block   => new Block_Type'(Block),
                                        Next    => Buffer.Head,
                                        Changed => True);
      else
         declare
            E : Entry_Ref_Type;
         begin
            E := Buffer.Head;
            loop
               if E.Address = Address then
                  Dealloc(E.Block);
                  E.Block := new Block_Type'(Block);
                  E.Changed := True;
                  return;
               end if;
               exit when E.Next = null or else Address < E.Next.Address;
               E := E.Next;
            end loop;

            E.Next := new Entry_Type'(Address => Address,
                                      Block   => new Block_Type'(Block),
                                      Next    => E.Next,
                                      Changed => True);
         end;
      end if;
   end Write;

end DB.IO.Blocks.Gen_Buffers;

