-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.IO.Blocks.CFS_IO is

   function "=" (N, M : Name_Type) return Boolean is
   begin
      return N.Len = M.Len and then
             N.Str(1 .. Natural(N.Len)) = M.str(1 .. Natural(M.Len));
   end "=";


   function "=" (N : Name_Type; S : String) return Boolean is
   begin
      return N.Len = S'Length and then N.Str(1 .. Natural(N.Len)) = S;
   end "=";


   function To_String (N : Name_Type) return String is
   begin
      return N.Str(1 .. Natural(N.Len));
   end To_String;


   function Disk_Name (ID : String) return Disk_Name_Type is
   begin
      for I in reverse ID'Range loop
         if ID(I) = '/' then
            declare
               Str : Name_String_Type;
            begin
               Str(1 .. I - ID'First) := ID(ID'First .. I - 1);
               return (Len => Name_Length_Type(I - ID'First), Str => Str);
            end;
         end if;
      end loop;
      raise IO_Error;
   end Disk_Name;


   function File_Name (ID : String) return File_Name_Type is
   begin
      for I in reverse ID'Range loop
         if ID(I) = '/' then
            declare
               Str : Name_String_Type;
            begin
               Str(1 .. ID'Last - I) := ID(I+1 .. ID'Last);
               return (Len => Name_Length_Type(ID'Length - I), Str => Str);
            end;
         end if;
      end loop;
      raise IO_Error;
   end File_Name;


   procedure Read_Super_Block
     (FD          : in  Low_Level.File_Descriptor_Type;
      Super_Block : out Super_Block_Ref_Type)
   is
      procedure Read_Chunk_Address is new Low_Level.Read(Chunk_Address_Type);
      procedure Read_Size is new Low_Level.Read(Size_Type);
      procedure Read_Files is new Low_Level.Read(File_Array_Type);
      Chunk_Count        : Chunk_Address_Type;
      Chunk_Size         : Size_Type;
      Files              : File_Array_Type;
   begin
      Low_Level.Seek(FD, 0);
      Read_Chunk_Address(FD, Chunk_Count);
      Read_Size(FD, Chunk_Size);
      Read_Files(FD, Files);
      declare
         use type Low_Level.File_Position_Type;
         type Definite_Chunk_Array_Type is new Chunk_Array_Type(1..Chunk_Count);
         procedure Read_Chunk_Array is
            new Low_Level.Read(Definite_Chunk_Array_Type);
         Chunks      : Definite_Chunk_Array_Type;
         Data_Offset : Size_Type;
      begin
         Read_Chunk_Array(FD, Chunks);
         Data_Offset := Size_Type((Low_Level.Current_File_Position(FD) +
                                   Low_level.File_Position_Type(Block_Size)-1) /
                                  Low_Level.File_Position_Type(Block_Size));
         Super_Block := new Super_Block_Type'
               (Chunk_Size         => Chunk_Size,
                Chunk_Count        => Chunk_Count,
                Files              => Files,
                Chunks             => Chunk_Array_Type(Chunks),
                Data_Offset        => Data_Offset);
      end;
   end Read_Super_Block;


   procedure Write_Super_Block
     (FD          : in Low_Level.File_Descriptor_Type;
      Super_Block : in Super_Block_Type)
   is
      use type Low_Level.File_Position_Type;
      type Definite_Chunk_Array_Type is
         new Chunk_Array_Type(1 .. Super_Block.Chunk_Count);
      procedure Write_Chunk_Address is new Low_Level.Write(Chunk_Address_Type);
      procedure Write_Size is new Low_Level.Write(Size_Type);
      procedure Write_Files is new Low_Level.Write(File_Array_Type);
      procedure Write_Chunk_Array is
         new Low_Level.Write(Definite_Chunk_Array_Type);
   begin
      Low_Level.Seek(FD, 0);
      Write_Chunk_Address(FD, Super_Block.Chunk_Count);
      Write_Size(FD, Super_Block.Chunk_Size);
      Write_Files(FD, Super_Block.Files);
      Write_Chunk_Array(FD, Definite_Chunk_Array_Type(Super_Block.Chunks));
   end Write_Super_Block;


   Disks : Disk_Handle_Array_Type
         := (others => (Name   => (Len => 0, others => <>), others => <>));


   procedure Get_Disk_Handle
     (Disk_Name : in  Disk_Name_Type;
      Disk      : out Disk_Handle_Ref_Type) is
   begin
      for I in Disks'Range loop
         declare
         begin
            Locks.Mutexes.Lock(Disks(I).Mutex);
            if Disks(I).Name = Disk_Name then
               Disk := Disks(I)'Access;
               Locks.Mutexes.Unlock(Disks(I).Mutex);
               return;
            end if;
            Locks.Mutexes.Unlock(Disks(I).Mutex);
         exception
            when others =>
               Locks.Mutexes.Unlock(Disks(I).Mutex);
         end;
      end loop;

      for I in Disks'Range loop
         declare
         begin
            Locks.Mutexes.Lock(Disks(I).Mutex);
            if Disks(I).Name = "" then
               Disk := Disks(I)'Access;
               Disk.Name := Disk_Name;
               Low_Level.Open(To_String(Disk.Name), Low_Level.Read_Write,
                              Disk.FD);
               Read_Super_Block(Disk.FD, Disk.Super_Block);
               Locks.Mutexes.Unlock(Disks(I).Mutex);
               return;
            end if;
            Locks.Mutexes.Unlock(Disks(I).Mutex);
         exception
            when others =>
               Locks.Mutexes.Unlock(Disks(I).Mutex);
         end;
      end loop;

      raise IO_Error;
   end Get_Disk_Handle;


   procedure Make_Filesystem
     (Device     : in String;
      Chunk_Size : in Size_Type)
   is
      pragma Assert (Chunk_Size >= Block_Size);
      pragma Assert (Chunk_Size mod Block_Size = 0);
      function Max_Chunk_Count
        (Pos        : Low_Level.File_Position_Type;
         Chunk_Size : Size_Type)
         return Chunk_Address_Type
      is
         use type Low_Level.File_Position_Type;
      begin
         return Chunk_Address_Type(Pos /
                                   Low_Level.File_Position_Type(Chunk_Size));
      end Max_Chunk_Count;

      use type Low_Level.File_Position_Type;
      FD  : Low_Level.File_Descriptor_Type;
      Pos : Low_Level.File_Position_Type;
   begin
      Low_Level.Open(Device, Low_Level.Read_Write, FD);
      Low_Level.Seek_End(FD, Pos);
      Pos := Pos - (Chunk_Address_Type'Size+7)/8
                 - (Size_Type'Size+7)/8
                 - (File_Array_Type'Size+7)/8
                 - (Low_Level.File_Position_Type(Max_Chunk_Count(Pos,Chunk_Size)
                    * (Chunk_Type'Size+7)/8));
      declare
         Chunk_Count : constant Chunk_Address_Type
                     := Max_Chunk_Count(Pos, Chunk_Size);
         No_Files    : constant File_Array_Type
                     := (others => (Len => 0, others => <>));
         Free_Chunks : constant Chunk_Array_Type(1 .. Chunk_Count)
                     := (others => (Owning_File_Index => Invalid_File_Index,
                                    others => <>));
         Super_Block : aliased constant Super_Block_Type
                     := (Chunk_Size  => Chunk_Size,
                         Chunk_Count => Chunk_Count,
                         Files       => No_Files,
                         Chunks      => Free_Chunks,
                         Data_Offset => 0);
      begin
         Write_Super_Block(FD, Super_Block);
      exception
         when others =>
            Low_Level.Close(FD);
      end;
      Low_Level.Close(FD);
   end Make_Filesystem;


   procedure Create
     (ID   : in  String;
      File : out File_Type)
   is
      File_Name  : constant File_Name_Type := CFS_IO.File_Name(ID);
      Disk       : Disk_Handle_Ref_Type;
      File_Index : File_Index_Type := Invalid_File_Index;
   begin
      Get_Disk_Handle(Disk_Name(ID), Disk);
      for I in Disk.Super_Block.Files'Range loop
         if Disk.Super_Block.Files(I) = File_Name then
            raise IO_Error;
         end if;
         if Disk.Super_Block.Files(I) = "" then
            File_Index := I;
         end if;
      end loop;

      if File_Index = Invalid_File_Index then
         raise IO_Error;
      end if;

      Disk.Super_Block.Files(File_Index) := File_Name;
      Write_Super_Block(Disk.FD, Disk.Super_Block.all);
      Open(ID, File);
   end Create;


   procedure Create_And_Open_Temporary
     (ID   : in  String;
      File : out File_Type) is
   begin
      raise IO_Error;
   end Create_And_Open_Temporary;


   procedure Open
     (ID   : in  String;
      File : out File_Type)
   is
      Disk       : Disk_Handle_Ref_Type;
      File_Name  : constant File_Name_Type  := CFS_IO.File_Name(ID);
      File_Index : File_Index_Type := File_Index_Type'Last;
   begin
      Get_Disk_Handle(Disk_Name(ID), Disk);
      for I in Disk.Super_Block.Files'Range loop
         if Disk.Super_Block.Files(I) = File_Name then
            File_Index := I;
         end if;
      end loop;

      if File_Index = Invalid_File_Index then
         raise IO_Error;
      end if;

      File.File_Index              := File_Index;
      File.Disk                    := Disk;
      File.Max_Address_Initialized := False;
      File.Max_Address             := Invalid_Address;
      declare
         SB : Super_Block_Ref_Type renames File.Disk.Super_Block;
         Chunk_Count : Size_Type := 0;
      begin
         for I in SB.Chunks'Range loop
            if SB.Chunks(I).Owning_File_Index = File_Index then
               Chunk_Count := Chunk_Count + 1;
            end if;
         end loop;
         File.Chunks := new Chunk_Address_Array_Type(1 .. Chunk_Count);
         for I in SB.Chunks'Range loop
            if SB.Chunks(I).Owning_File_Index = File_Index then
               File.Chunks(SB.Chunks(I).Index_In_File) := I;
            end if;
         end loop;
      end;
   end Open;


   procedure Allocate_Chunk
     (File : in out File_Type)
   is
      SB : Super_Block_Ref_Type renames File.Disk.Super_Block;
   begin
      Locks.Mutexes.Lock(File.Disk.Mutex);
      for I in SB.Chunks'Range loop
         if SB.Chunks(I).Owning_File_Index =
               Invalid_File_Index then
            SB.Chunks(I) := (Owning_File_Index => File.File_Index,
                             Index_In_File     => File.Chunks'Last + 1);
            Write_Super_Block(File.Disk.FD, SB.all);
            declare
               procedure Free is new Ada.Unchecked_Deallocation
                  (Chunk_Address_Array_Type, Chunk_Address_Array_Ref_Type);
               Chunks : constant Chunk_Address_Array_Ref_Type
                      := new Chunk_Address_Array_Type
                                 (File.Chunks'First .. File.Chunks'Last+1);
            begin
               Chunks(File.Chunks'Range)      := File.Chunks.all;
               Chunks(File.Chunks'Length + 1) := I;
               Free(File.Chunks);
               File.Chunks := Chunks;
            end;
            Locks.Mutexes.Unlock(File.Disk.Mutex);
            return;
         end if;
      end loop;
      Locks.Mutexes.Unlock(File.Disk.Mutex);
      raise IO_Error;
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Disk.Mutex);
         raise;
   end Allocate_Chunk;


   procedure Set_Block_Count
     (File    : in out File_Type;
      Address : in     Address_Type) is
   begin
      File.Max_Address_Initialized := True;
      File.Max_Address             := Address;
   end Set_Block_Count;


   procedure Close
     (File : in out File_Type) is
   begin
      null;
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
      return Address;
   end To_Valid_Address;


   function Is_Valid_Address
     (Address : Address_Type)
      return Boolean is
   begin
      return Address /= Invalid_Address;
   end Is_Valid_Address;


   procedure Convert_Block_Address_To_Disk_Position
     (File           : in out File_Type;
      Address        : in     Valid_Address_Type;
      Disk_Pos       :    out Low_Level.File_Position_Type;
      Allow_Allocate : in     Boolean)
   is
      Chunk_Size           : constant Size_Type
                           := File.Disk.Super_Block.Chunk_Size;
      pragma Assert (Chunk_Size mod Block_Size = 0);
      Chunk_Size_In_Blocks : constant Valid_Address_Type
                           := Valid_Address_Type(Chunk_Size / Block_Size);
      Chunk_Number         : constant Chunk_Number_Type
                           := Chunk_Number_Type(Address /
                                                Chunk_Size_In_Blocks) +
                              File.Chunks'First;
   begin
      if Chunk_Number not in File.Chunks'Range then
         if Allow_Allocate then
            Allocate_Chunk(File);
         else
            raise IO_Error;
         end if;
      end if;
      declare
         use type Low_Level.File_Position_Type;
         Data_Offset      : constant Size_Type
                          := File.Disk.Super_Block.Data_Offset;
         Data_Offset_Pos  : constant Low_Level.File_Position_Type
                          := Low_Level.File_Position_Type(Data_Offset) *
                             Low_Level.File_Position_Type(Block_Size);
         Chunk_Address    : constant Chunk_Address_Type
                          := File.Chunks(Chunk_Number);
         Chunk_Pos        : constant Low_Level.File_Position_Type
                          := (Low_Level.File_Position_Type(Chunk_Address)-1) *
                             Low_Level.File_Position_Type(Chunk_Size);
         Block_In_Chunk   : constant Address_Type
                          := Address mod Chunk_Size_In_Blocks;
         Chunk_Offset_Pos : constant Low_Level.File_Position_Type
                          := Low_Level.File_Position_Type(Block_In_Chunk) *
                             Low_Level.File_Position_Type(Block_Size);
      begin
         Disk_Pos := Data_Offset_Pos + Chunk_Pos + Chunk_Offset_Pos;
      end;
   end Convert_Block_Address_To_Disk_Position;


   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type)
   is
      procedure LL_Read is new Low_Level.PRead(Block_Type);
      Pos : Low_Level.File_Position_Type;
   begin
      Locks.Mutexes.Lock(File.Mutex);

      if File.Max_Address_Initialized and then
         File.Max_Address < Address then
         raise IO_Error;
      end if;
      Convert_Block_Address_To_Disk_Position(File, Address, Pos,
                                             Allow_Allocate => False);
      LL_Read(File.Disk.FD, Pos, Block);

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
      procedure LL_Write is new Low_Level.PWrite(Block_Type);
      Pos : Low_Level.File_Position_Type;
   begin
      Locks.Mutexes.Lock(File.Mutex);

      Convert_Block_Address_To_Disk_Position(File, Address, Pos,
                                             Allow_Allocate => True);
      LL_Write(File.Disk.FD, Pos, Block);
      if not File.Max_Address_Initialized
      or else File.Max_Address < Address then
         File.Max_Address := Address;
      end if;

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

      if not File.Max_Address_Initialized then
         raise IO_Error;
      end if;
      File.Max_Address := File.Max_Address + 1;
      Address := File.Max_Address;

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

end DB.IO.Blocks.CFS_IO;

