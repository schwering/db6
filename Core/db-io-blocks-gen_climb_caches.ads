with System.Pool_Global;
with System.Storage_Pools;

use System.Pool_Global;
use System.Storage_Pools;

with DB.IO.Blocks.Gen_IO;
with DB.Locks.Mutexes;
with DB.Utils;
with DB.Utils.Gen_Hashtables;

generic
   with package P_IO is new Gen_IO (<>);
package DB.IO.Blocks.Gen_Climb_Caches is

   subtype Address_Type is P_IO.Address_Type;
   subtype Valid_Address_Type is P_IO.Valid_Address_Type;
   subtype Ticket_Type is P_IO.Ticket_Type;

   Mega_Bytes  : constant := 2**(10 + 10) / Block_Size;
   Buffer_Size : constant := 1 * Mega_Bytes;

   Invalid_Address : constant Address_Type := P_IO.Invalid_Address;
   Needs_Explicit_Block_Count : constant Boolean
      := P_IO.Needs_Explicit_Block_Count;

   type Block_Ref_Type is access Block_Type;
   pragma Controlled (Block_Ref_Type);

   subtype Length_Type is Natural range 0 .. Buffer_Size;
   subtype Index_Type is Length_Type range 1 .. Length_Type'Last;

   function Hash (A : Valid_Address_Type) return Utils.Hash_Type;
   function Rehash (H : Utils.Hash_Type) return Utils.Hash_Type;

   package HT is new Utils.Gen_Hashtables
     (Key_Type     => Valid_Address_Type,
      Value_Type   => Index_Type,
      Hash         => Hash,
      Rehash       => Rehash,
      "="          => P_IO."=",
      Storage_Pool => Root_Storage_Pool'Class(Global_Pool_Object));
   type Table_Ref_Type is access HT.Table_Type;

   type Buffer_Element_Type is
      record
         Address : Valid_Address_Type;
         Block   : Block_Ref_Type;
         Dirty   : Boolean := False;
      end record;
   type Buffer_Type is array (Index_Type) of Buffer_Element_Type;


   type File_Type is limited
      record
         File         : P_IO.File_Type;
         Mutex        : Locks.Mutexes.Mutex_Type;
         Buffer       : Buffer_Type;
         Hash_Table   : Table_Ref_Type     := null;
         Length       : Length_Type        := 0;
         Next_Address : Valid_Address_Type := P_IO.First;
      end record;

   procedure Create
     (ID   : in  String;
      File : out File_Type);

   procedure Open
     (ID   : in  String;
      File : out File_Type);

   procedure Close
     (File : in out File_Type);

   procedure Set_Block_Count
     (File    : in out File_Type;
      Address : in     Address_Type);

   function First
      return Valid_Address_Type
   renames P_IO.First;

   function Succ
     (Address : Valid_Address_Type)
      return Valid_Address_Type
   renames P_IO.Succ;

   function "<"
     (A, B : Valid_Address_Type)
      return Boolean
   renames P_IO."<";

   function "="
     (A, B : Valid_Address_Type)
      return Boolean
   renames P_IO."=";

   function Image
     (A : in Valid_Address_Type)
      return String
   renames P_IO.Image;

   function To_Address
     (Address : Valid_Address_Type)
      return Address_Type
   renames P_IO.To_Address;

   function To_Valid_Address
     (Address : Address_Type)
      return Valid_Address_Type
   renames P_IO.To_Valid_Address;

   function Is_Valid_Address
     (Address : Address_Type)
      return Boolean
   renames P_IO.Is_Valid_Address;

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
      Ticket :    out Ticket_Type);

   procedure Release_Ticket
     (File   : in out File_Type;
      Ticket : in     Ticket_Type);

   procedure Read_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type);

   procedure Write_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type);

   procedure Certify_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type);

   procedure Unlock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type);


   package IO is new Gen_IO
     (P_File_Type                  => File_Type,
      P_Ticket_Type                => Ticket_Type,
      P_Address_Type               => Address_Type,
      P_Needs_Explicit_Block_Count => Needs_Explicit_Block_Count,
      P_Valid_Address_Type         => Valid_Address_Type,
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
   pragma Inline (Hash);
   pragma Inline (Rehash);
   pragma Inline (Read);
   pragma Inline (Write);

end DB.IO.Blocks.Gen_Climb_Caches;

