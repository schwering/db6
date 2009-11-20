-- Abstract:
--
-- Volatile IO implementation that compresses the blocks with DEFLATE and stores
-- the buffers in main memory.
--
-- Copyright 2008, 2009 Christoph Schwering

with DB.Compression.Deflate;
with DB.IO.Blocks.Gen_IO;
with DB.Locks.Mutexes;
with DB.Locks.Semaphores;

package DB.IO.Blocks.Compressed_Memory_IO is
   pragma Preelaborate;

   type Address_Type is new Natural;
   subtype Valid_Address_Type is Address_Type range 1 .. Address_Type'Last;
   type File_Object_Type is limited private;
   type File_Type is access File_Object_Type;
   pragma Controlled (File_Type);

   Invalid_Address : constant Address_Type := 0;

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
     (File : in out File_Type)
   is null;

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


   subtype Count_Type is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   function Byte_Count return Count_Type;
   function Block_Count return Count_Type;


private
   subtype Buffer_Size_Type is Compression.Deflate.Size_Type;
   subtype Buffer_Type is Compression.Deflate.Buffer_Type;

   type Buffer_Ref_Type is access Buffer_Type;
   pragma Controlled (Buffer_Ref_Type);

   type Buffer_Ref_Array_Type is
      array (Valid_Address_Type range <>) of Buffer_Ref_Type;

   type Buffer_Ref_Array_Ref_Type is access Buffer_Ref_Array_Type;
   pragma Controlled (Buffer_Ref_Array_Ref_Type);

   type File_Object_Type is limited
      record
         Buffer    : Buffer_Ref_Array_Ref_Type := null;
         Capacity  : Address_Type := 0;
         Current   : Address_Type := 0;
         Maximum   : Address_Type := 0;
         Mutex     : Locks.Mutexes.Mutex_Type; -- for atom. of seek+read/write
         Semaphore : Locks.Semaphores.Semaphore_Type;
      end record;

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

end DB.IO.Blocks.Compressed_Memory_IO;

