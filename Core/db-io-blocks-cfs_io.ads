-- Abstract:
--
-- Chunk File System (CFS) IO. CFS is a simple file system.
-- Files grow by chunks which are pretty large in size (e.g. 128 MB).
-- The file system is flat and files have no meta data except for their name.
--
-- Design Notes:
--
-- Each partition starts with a super block. The super block stores the chunk
-- size of this partition, the total count of chunks and the current files.
-- A file is defined only by its name and the sequence of chunks that belongs to
-- it.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.IO.Low_Level;
with DB.IO.Blocks.Gen_IO;
with DB.Locks.Mutexes;
with DB.Locks.Semaphores;

package DB.IO.Blocks.CFS_IO is
   pragma Preelaborate;

   Max_Name_Length      : constant := 16;
   Max_Count_Files      : constant := 128;
   Max_Count_Open_Disks : constant := 4;


   type Address_Type is new Natural;
   subtype Valid_Address_Type is Address_Type range 1 .. Address_Type'Last;

   Invalid_Address : constant Address_Type := 0;

   type File_Index_Type is range 0 .. Max_Count_Files;
   subtype Valid_File_Index_Type is
      File_Index_Type range 1 .. File_Index_Type'Last;

   Invalid_File_Index : constant File_Index_Type := 0;

   subtype Chunk_Number_Type is Size_Type;
   type Chunk_Address_Type is new Positive;
   type Chunk_Address_Array_Type is
      array (Chunk_Number_Type range <>) of Chunk_Address_Type;
   type Chunk_Address_Array_Ref_Type is access Chunk_Address_Array_Type;

   type Disk_Handle_Type (<>) is limited private;
   type Disk_Handle_Ref_Type is access all Disk_Handle_Type;

   type File_Type is limited -- not private due to Gen_IO package instance
      record
         File_Index              : Valid_File_Index_Type;
         Disk                    : Disk_Handle_Ref_Type;
         Chunks                  : Chunk_Address_Array_Ref_Type;
         Mutex                   : Locks.Mutexes.Mutex_Type;
         Semaphore               : Locks.Semaphores.Semaphore_Type;
         Max_Address_Initialized : Boolean      := False;
         Max_Address             : Address_Type := Invalid_Address;
      end record;


   procedure Make_Filesystem
     (Device     : in String;
      Chunk_Size : in Size_Type);

   procedure Create
     (ID   : in  String;
      File : out File_Type);

   procedure Create_And_Open_Temporary
     (ID   : in  String;
      File : out File_Type);

   procedure Open
     (ID   : in  String;
      File : out File_Type);

   procedure Set_Block_Count
     (File    : in out File_Type;
      Address : in     Address_Type);

   procedure Close
     (File : in out File_Type);

   function First
      return Valid_Address_Type;

   function Succ
     (Address : Valid_Address_Type)
      return Valid_Address_Type;

   function Image
     (A : in Valid_Address_Type)
      return String;

   function To_Address
     (Address : Valid_Address_Type)
      return Address_Type;

   function To_Valid_Address
     (Address : Address_Type)
      return Valid_Address_Type;

   function Is_Valid_Address
     (Address : Address_Type)
      return Boolean;

   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type);

   procedure Write
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   : in     Block_Type);

   procedure Seek_New
     (File    : in out File_Type;
      Address :    out Address_Type);

   procedure Acquire_Ticket
     (File   : in out File_Type;
      Ticket :    out Locks.Semaphores.Ticket_Type);

   procedure Release_Ticket
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type);

   procedure Read_Lock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type);

   procedure Write_Lock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type);

   procedure Certify_Lock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type);

   procedure Unlock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type);


   package IO is new Gen_IO
     (File_Type                  => File_Type,
      Ticket_Type                => Locks.Semaphores.Ticket_Type,
      Address_Type               => Address_Type,
      Valid_Address_Type         => Valid_Address_Type,
      Needs_Explicit_Block_Count => True,
      Invalid_Address            => Invalid_Address,
      Create                     => Create,
      Create_And_Open_Temporary  => Create_And_Open_Temporary,
      Open                       => Open,
      Set_Block_Count            => Set_Block_Count,
      Close                      => Close,
      Succ                       => Succ,
      "<"                        => "<",
      "="                        => "=",
      Image                      => Image,
      First                      => First,
      To_Address                 => To_Address,
      To_Valid_Address           => To_Valid_Address,
      Is_Valid_Address           => Is_Valid_Address,
      Read                       => Read,
      Write                      => Write,
      Seek_New                   => Seek_New,
      Acquire_Ticket             => Acquire_Ticket,
      Release_Ticket             => Release_Ticket,
      Read_Lock                  => Read_Lock,
      Write_Lock                 => Write_Lock,
      Certify_Lock               => Certify_Lock,
      Unlock                     => Unlock);

private
   type Name_Length_Type is range 0 .. Max_Name_Length;
   subtype Name_String_Type is String(1 .. Max_Name_Length);
   type Name_Type is
      record
         Len : Name_Length_Type;
         Str : Name_String_Type;
      end record;
   subtype Disk_Name_Type is Name_Type;
   subtype File_Name_Type is Name_Type;


   type File_Array_Type is array (Valid_File_Index_Type) of File_Name_Type;
   type Chunk_Type is
      record
         Owning_File_Index : File_Index_Type;
         Index_In_File     : Chunk_Number_Type;
      end record;
   type Chunk_Array_Type is array (Chunk_Address_Type range <>) of Chunk_Type;
   type Super_Block_Type (Chunk_Size  : Size_Type;
                          Chunk_Count : Chunk_Address_Type) is limited
      record
         Files       : File_Array_Type;
         Chunks      : Chunk_Array_Type(1 .. Chunk_Count);
         Data_Offset : Size_Type;
      end record;
   type Super_Block_Ref_Type is access Super_Block_Type;

   type Disk_Handle_Type is limited
      record
         Name        : Disk_Name_Type;
         FD          : Low_Level.File_Descriptor_Type;
         Super_Block : Super_Block_Ref_Type;
         Mutex       : Locks.Mutexes.Mutex_Type;
      end record;
   type Disk_Handle_Array_Type is
      array (Positive range 1 ..  Max_Count_Open_Disks)
      of aliased Disk_Handle_Type;

   pragma Inline (Succ);
   pragma Inline (Image);
   pragma Inline (First);
   pragma Inline (To_Address);
   pragma Inline (To_Valid_Address);
   pragma Inline (Is_Valid_Address);
   pragma Inline (Read);
   pragma Inline (Write);
   pragma Inline (Seek_New);
   pragma Inline (Acquire_Ticket);
   pragma Inline (Release_Ticket);
   pragma Inline (Read_Lock);
   pragma Inline (Write_Lock);
   pragma Inline (Certify_Lock);
   pragma Inline (Unlock);

end DB.IO.Blocks.CFS_IO;

