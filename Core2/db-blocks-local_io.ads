-- Abstract:
--
-- Normal file IO.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks.Low_Level_IO;
with DB.Blocks.Gen_IO_Signature;
with DB.Locks.Gen_Mutex_Sets;

package DB.Blocks.Local_IO is
   pragma Preelaborate;

   type Address_Type is new Natural;
   subtype Valid_Address_Type is Address_Type range 1 .. Address_Type'Last;

   Invalid_Address : constant Address_Type := 0;
   First_Address   : constant Address_Type := 1;

   package Mutex_Sets is new Locks.Gen_Mutex_Sets
     (Item_Type           => Valid_Address_Type,
      "<"                 => "<",
      "="                 => "=");

   type File_Type is limited
      record
         FD        : Low_Level_IO.File_Descriptor_Type;
         Mutex_Set : Mutex_Sets.Mutex_Set_Type;
      end record;

   procedure Create (ID : in String; File : out File_Type);

   procedure Create_And_Open_Temporary (ID : in String; File : out File_Type);

   procedure Open (ID : in String; File : out File_Type);

   procedure Close (File : in out File_Type);

   procedure Unlink (ID : in String);

   function Succ
     (Address : Valid_Address_Type)
      return Valid_Address_Type;

   function Image (A : Valid_Address_Type) return String;

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
     (File           : in out File_Type;
      Address        : in     Valid_Address_Type;
      Block          : in     Block_Type;
      Cache_Priority : in     Natural := Natural'First);

   procedure Write_New_Block
     (File           : in out File_Type;
      Address        :    out Valid_Address_Type;
      Block          : in     Block_Type;
      Cache_Priority : in     Natural := Natural'First);

   procedure Try_Lock
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Success :    out Boolean);

   procedure Lock
     (File    : in out File_Type;
      Address : in     Valid_Address_Type);

   procedure Unlock
     (File    : in out File_Type;
      Address : in     Valid_Address_Type);

   function FD
     (File : File_Type)
      return Low_Level_IO.File_Descriptor_Type;


   package IO_Signature is new Gen_IO_Signature
     (File_Type                  => File_Type,
      Address_Type               => Address_Type,
      Valid_Address_Type         => Valid_Address_Type,
      Invalid_Address            => Invalid_Address,
      First_Address              => First_Address,
      Succ                       => Succ,
      "<"                        => "<",
      "="                        => "=",
      Image                      => Image,
      To_Address                 => To_Address,
      To_Valid_Address           => To_Valid_Address,
      Is_Valid_Address           => Is_Valid_Address,
      Create                     => Create,
      Create_And_Open_Temporary  => Create_And_Open_Temporary,
      Open                       => Open,
      Close                      => Close,
      Unlink                     => Unlink,
      Read                       => Read,
      Write                      => Write,
      Write_New_Block            => Write_New_Block,
      Lock                       => Lock,
      Unlock                     => Unlock);

private
   pragma Inline (Succ);
   pragma Inline (Image);
   pragma Inline (To_Address);
   pragma Inline (To_Valid_Address);
   pragma Inline (Is_Valid_Address);
   pragma Inline (Read);
   pragma Inline (Write);
   pragma Inline (Write_New_Block);
   pragma Inline (Lock);
   pragma Inline (Unlock);

end DB.Blocks.Local_IO;

