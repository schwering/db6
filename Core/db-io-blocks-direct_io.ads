-- Abstract:
--
-- Normal file IO without operating system IO cache.
--
-- Copyright 2008, 2009 Christoph Schwering

with DB.IO.Low_Level;
with DB.IO.Blocks.Gen_IO;
with DB.IO.Blocks.Gen_Climb_Caches;
with DB.IO.Blocks.Gen_System_Locking_IO;
with DB.Locks.Mutexes;
with DB.Locks.Semaphores;

package DB.IO.Blocks.Direct_IO is

   type Address_Type is new Natural;
   subtype Valid_Address_Type is Address_Type range 1 .. Address_Type'Last;

   Invalid_Address : constant Address_Type := 0;

   type File_Type is limited
      record
         FD        : Low_Level.File_Descriptor_Type;
         Mutex     : Locks.Mutexes.Mutex_Type;
         Semaphore : Locks.Semaphores.Semaphore_Type;
      end record;


   procedure Create
     (ID   : in  String;
      File : out File_Type);

   procedure Open
     (ID   : in  String;
      File : out File_Type);

   procedure Set_Block_Count
     (File    : in out File_Type;
      Address : in     Address_Type)
   is null;

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
      Address :    out Valid_Address_Type);

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

   function FD
     (File : File_Type)
      return Low_Level.File_Descriptor_Type;


   package IO is new Gen_IO
     (File_Type                  => File_Type,
      Ticket_Type                => Locks.Semaphores.Ticket_Type,
      Address_Type               => Address_Type,
      Valid_Address_Type         => Valid_Address_Type,
      Needs_Explicit_Block_Count => False,
      Invalid_Address            => Invalid_Address,
      Create                     => Create,
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

   package Climb_Cache is new Gen_Climb_Caches(IO);
   package Climb_Cached_IO renames Climb_Cache.IO;

   package System_Locking is new Gen_System_Locking_IO(IO, FD);
   package System_Locking_IO renames System_Locking.IO;

private
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

end DB.IO.Blocks.Direct_IO;

