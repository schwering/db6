-- Abstract:
--
-- Main memory IO implementation.
--
-- Copyright 2008, 2009 Christoph Schwering

with DB.IO.Blocks.Gen_IO;
with DB.Locks.Mutexes;
with DB.Locks.Semaphores;

package DB.IO.Blocks.Memory_IO is
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
     (P_File_Type                  => File_Type,
      P_Ticket_Type                => Locks.Semaphores.Ticket_Type,
      P_Address_Type               => Address_Type,
      P_Valid_Address_Type         => Valid_Address_Type,
      P_Needs_Explicit_Block_Count => True,
      P_Invalid_Address            => Invalid_Address,
      P_Create                     => Create,
      P_Open                       => Open,
      P_Set_Block_Count            => Set_Block_Count,
      P_Close                      => Close,
      P_Succ                       => Succ,
      P_Is_Less                    => "<",
      P_Is_Equal                   => "=",
      P_Image                      => Image,
      P_First                      => First,
      P_To_Address                 => To_Address,
      P_To_Valid_Address           => To_Valid_Address,
      P_Is_Valid_Address           => Is_Valid_Address,
      P_Read                       => Read,
      P_Write                      => Write,
      P_Seek_New                   => Seek_New,
      P_Acquire_Ticket             => Acquire_Ticket,
      P_Release_Ticket             => Release_Ticket,
      P_Read_Lock                  => Read_Lock,
      P_Write_Lock                 => Write_Lock,
      P_Certify_Lock               => Certify_Lock,
      P_Unlock                     => Unlock);


private
   type Block_Ref_Type is access Block_Type;
   pragma Controlled (Block_Ref_Type);
   type Block_Ref_Array_Type is
      array (Valid_Address_Type range <>) of Block_Ref_Type;
   type Block_Ref_Array_Ref_Type is access Block_Ref_Array_Type;
   pragma Controlled (Block_Ref_Array_Ref_Type);
   type File_Object_Type is limited
      record
         Buffer    : Block_Ref_Array_Ref_Type := null;
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

end DB.IO.Blocks.Memory_IO;

