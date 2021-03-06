-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.IO.Blocks.Memory_IO is

   Max_File_Count : constant := 10;

   type Entry_Type (Name_Length : Positive) is
      record
         Name : String(1 .. Name_Length);
         File : File_Type;
      end record;
   type Entry_Ref_Type is access Entry_Type;
   type Entry_Ref_Array_Type is
      array (Positive range 1 .. Max_File_Count) of Entry_Ref_Type;

   Files : Entry_Ref_Array_Type := (others => null);

   procedure Create
     (ID   : in  String;
      File : out File_Type) is
   begin
      for I in Files'Range loop
         if Files(I) /= null and then Files(I).Name = ID then
            raise IO_Error;
         elsif Files(I) = null then
            File := new File_Object_Type;
            Files(I) := new Entry_Type'(Name_Length => ID'Length,
                                        Name        => ID,
                                        File        => File);
            return;
         end if;
      end loop;
      raise IO_Error;
   end Create;


   procedure Create_And_Open_Temporary
     (ID   : in  String;
      File : out File_Type) is
   begin
      raise IO_Error;
   end Create_And_Open_Temporary;


   procedure Open
     (ID   : in  String;
      File : out File_Type) is
   begin
      for I in Files'Range loop
         if Files(I) /= null and then Files(I).Name = ID then
            File := Files(I).File;
            return;
         end if;
      end loop;
      raise IO_Error;
   end Open;


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
      return Address;
   end To_Valid_Address;


   function Is_Valid_Address
     (Address : Address_Type)
      return Boolean is
   begin
      return Address /= Invalid_Address;
   end Is_Valid_Address;


   procedure Resize_Buffer
     (File : in File_Type) is
   begin
      if File.Current >= File.Capacity then
         declare
            Capacity : constant Address_Type
                     := File.Capacity * 4 / 3 + 1;
            Buffer   : constant Block_Ref_Array_Ref_Type
                     := new Block_Ref_Array_Type(1 .. Capacity);
         begin
            if File.Buffer /= null then
               Buffer(File.Buffer'Range) := File.Buffer(File.Buffer'Range);
            end if;
            for I in File.Capacity + 1 .. Capacity loop
               Buffer(I) := new Block_Type;
            end loop;
            File.Capacity := Capacity;
            File.Buffer := Buffer;
         end;
      end if;
   end Resize_Buffer;


   procedure Next_Block
     (File : in File_Type) is
   begin
      File.Current := File.Current + 1;
      Resize_Buffer(File);
   end Next_Block;


   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type) is
   begin
      Locks.Mutexes.Lock(File.Mutex);

      File.Current := Address;

      if File.Current > File.Maximum then
         raise IO_Error;
      end if;

      Block := File.Buffer(File.Current).all;
      Next_Block(File);

      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Read;


   procedure Write
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   : in     Block_Type) is
   begin
      Locks.Mutexes.Lock(File.Mutex);

      File.Current := Address;

      Resize_Buffer(File);
      File.Buffer(File.Current).all := Block;
      if File.Maximum < File.Current then
         File.Maximum := File.Current;
      end if;
      Next_Block(File);

      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Write;


   procedure Seek_New
     (File    : in out File_Type;
      Address :    out Address_Type) is
   begin
      Locks.Mutexes.Lock(File.Mutex);

      File.Current := File.Maximum + 1;
      Address      := File.Current;

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

end DB.IO.Blocks.Memory_IO;

