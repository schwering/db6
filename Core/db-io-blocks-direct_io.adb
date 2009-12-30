-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.IO.Blocks.Direct_IO is
 
   function To_Valid_Address
     (Position : Low_Level.File_Position_Type)
      return Valid_Address_Type;


   procedure Create
     (ID   : in  String;
      File : out File_Type)
   is
      Last_Position : Low_Level.File_Position_Type;
   begin
      Low_Level.Open_Direct(Path      => ID,
                            Open_Kind => Low_Level.Create,
                            File      => File.FD);
      pragma Warnings (Off, Last_Position);
      Low_Level.Get_Size(File          => File.FD,
                         Last_Position => Last_Position);
   end Create;


   procedure Create_And_Open_Temporary
     (ID   : in  String;
      File : out File_Type) is
   begin
      declare
      begin
         Create(ID, File);
      exception
         when others =>
            Low_Level.Unlink(ID);
            raise;
      end;
      Low_Level.Unlink(ID);
   end Create_And_Open_Temporary;


   procedure Open
     (ID   : in  String;
      File : out File_Type) is
   begin
      Low_Level.Open_Direct(Path      => ID,
                            Open_Kind => Low_Level.Read_Write,
                            File      => File.FD);
   end Open;


   procedure Close
     (File : in out File_Type) is
   begin
      Low_Level.Close(File.FD);
   end Close;


   function First
      return Valid_Address_Type is
   begin
      return 1;
   end First;


   function Succ
     (Address : Valid_Address_Type)
      return Valid_Address_Type is
   begin
      return Address + 1;
   end Succ;


   function Image
     (A : in Valid_Address_Type)
      return String is
   begin
      return Valid_Address_Type'Image(A);
   end Image;


   function To_Address
     (Address : Valid_Address_Type)
      return Address_Type is
   begin
      return Address;
   end To_Address;


   function To_Valid_Address
     (Address : Address_Type)
      return Valid_Address_Type is
   begin
      if Address not in Valid_Address_Type then
         raise IO_Error;
      end if;
      return Address;
   end To_Valid_Address;


   function Is_Valid_Address
     (Address : Address_Type)
      return Boolean is
   begin
      return Address in Valid_Address_Type;
   end Is_Valid_Address;


   function To_Valid_Address
     (Position : Low_Level.File_Position_Type)
      return Valid_Address_Type
   is
      pragma Inline (To_Valid_Address);
      use type Low_Level.File_Position_Type;
   begin
      if Position mod Block_Size /= 0 then
         raise IO_Error;
      end if;
      return Valid_Address_Type(Position / Block_Size + 1);
   end To_Valid_Address;


   function To_File_Position
     (Address : Valid_Address_Type)
      return Low_Level.File_Position_Type
   is
      pragma Inline (To_File_Position);
      use type Low_Level.File_Position_Type;
   begin
      if not Address'Valid then
         raise IO_Error;
      end if;
      return Low_Level.File_Position_Type(Address - 1) * Block_Size;
   end To_File_Position;


   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type)
   is
      procedure LL_Read is new Low_Level.Read_Direct(Block_Type);
   begin
      Locks.Mutexes.Lock(File.Mutex);
      Low_Level.Seek(File.FD, To_File_Position(Address));
      LL_Read(File.FD, Block);
      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Read;


   procedure Write
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   : in     Block_Type)
   is
      procedure LL_Write is new Low_Level.Write_Direct(Block_Type);
   begin
      Locks.Mutexes.Lock(File.Mutex);
      Low_Level.Seek(File.FD, To_File_Position(Address));
      LL_Write(File.FD, Block);
      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Write;


   procedure Seek_New
     (File    : in out File_Type;
      Address :    out Valid_Address_Type)
   is
      Pos : Low_Level.File_Position_Type;
   begin
      Locks.Mutexes.Lock(File.Mutex);
      Low_Level.Seek_End(File.FD, Pos);
      Address := To_Valid_Address(Pos);
      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Seek_New;


   procedure Acquire_Ticket
     (File   : in out File_Type;
      Ticket :    out Locks.Semaphores.Ticket_Type) is
   begin
      Locks.Semaphores.Acquire_Ticket(File.Semaphore, Ticket);
   end Acquire_Ticket;


   procedure Release_Ticket
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type) is
   begin
      Locks.Semaphores.Release_Ticket(File.Semaphore, Ticket);
   end Release_Ticket;


   procedure Read_Lock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type) is
   begin
      Locks.Semaphores.Read_Lock(File.Semaphore, Ticket);
   end Read_Lock;


   procedure Write_Lock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type) is
   begin
      Locks.Semaphores.Write_Lock(File.Semaphore, Ticket);
   end Write_Lock;


   procedure Certify_Lock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type) is
   begin
      Locks.Semaphores.Certify_Lock(File.Semaphore, Ticket);
   end Certify_Lock;


   procedure Unlock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type) is
   begin
      Locks.Semaphores.Unlock(File.Semaphore, Ticket);
   end Unlock;


   function FD
     (File : File_Type)
      return Low_Level.File_Descriptor_Type is
   begin
      return File.FD;
   end FD;

end DB.IO.Blocks.Direct_IO;

