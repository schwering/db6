-- Abstract:
--
-- Normal file IO.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks.Low_Level_IO;
with DB.Blocks.Gen_IO_Signature;

package DB.Blocks.Local_IO is
   pragma Elaborate_Body;

   type Address_Type is new Natural;
   subtype Valid_Address_Type is Address_Type range 1 .. Address_Type'Last;

   Invalid_Address : constant Address_Type := 0;

   type File_Type is limited
      record
         FD : Low_Level_IO.File_Descriptor_Type;
      end record;

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

   procedure Allocate
     (File    : in out File_Type;
      Address :    out Valid_Address_Type);

   procedure Lock
     (File    : in out File_Type;
      Address : in     Address_Type);

   procedure Unlock
     (File    : in out File_Type;
      Address : in     Address_Type);

   function FD
     (File : File_Type)
      return Low_Level_IO.File_Descriptor_Type;


   package IO_Signature is new Gen_IO_Signature
     (File_Type                  => File_Type,
      Address_Type               => Address_Type,
      Valid_Address_Type         => Valid_Address_Type,
      Needs_Explicit_Block_Count => False,
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
      Allocate                   => Allocate,
      Lock                       => Lock,
      Unlock                     => Unlock);

private
   pragma Inline (Succ);
   pragma Inline (Image);
   pragma Inline (First);
   pragma Inline (To_Address);
   pragma Inline (To_Valid_Address);
   pragma Inline (Is_Valid_Address);
   pragma Inline (Read);
   pragma Inline (Write);
   pragma Inline (Allocate);
   pragma Inline (Lock);
   pragma Inline (Unlock);

end DB.Blocks.Local_IO;

