-- Abstract:
--
-- Main memory IO implementation.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks.Gen_IO_Signature;
with DB.Locks.Mutexes;
with DB.Locks.Gen_Mutex_Sets;
with DB.Utils;

package DB.Blocks.Memory_IO is
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

   procedure Create_And_Open_Temporary
     (ID   : in  String;
      File : out File_Type);

   procedure Open
     (ID   : in  String;
      File : out File_Type);

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

   procedure Allocate
     (File    : in out File_Type;
      Address :    out Address_Type);

   procedure Lock
     (File    : in out File_Type;
      Address : in     Valid_Address_Type);

   procedure Unlock
     (File    : in out File_Type;
      Address : in     Valid_Address_Type);


   package IO is new Gen_IO_Signature
     (File_Type                  => File_Type,
      Address_Type               => Address_Type,
      Valid_Address_Type         => Valid_Address_Type,
      Invalid_Address            => Invalid_Address,
      Create                     => Create,
      Create_And_Open_Temporary  => Create_And_Open_Temporary,
      Open                       => Open,
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
   type Block_Ref_Type is access Block_Type;
   pragma Controlled (Block_Ref_Type);

   type Block_Ref_Array_Type is
      array (Valid_Address_Type range <>) of Block_Ref_Type;

   type Block_Ref_Array_Ref_Type is access Block_Ref_Array_Type;
   pragma Controlled (Block_Ref_Array_Ref_Type);

   function Hash(A : Address_Type) return Utils.Hash_Type;

   package Mutex_Sets is new Locks.Gen_Mutex_Sets
     (Item_Type           => Address_Type,
      "="                 => "=",
      Hash                => Hash,
      Invalid_Item        => Invalid_Address,
      Hashtable_Size      => 30);

   type File_Object_Type is limited
      record
         Buffer    : Block_Ref_Array_Ref_Type := null;
         Capacity  : Address_Type := 0;
         Current   : Address_Type := 0;
         Maximum   : Address_Type := 0;
         Mutex     : Locks.Mutexes.Mutex_Type; -- for atom. of seek+read/write
         Mutex_Set : Mutex_Sets.Mutex_Set_Type;
      end record;

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

end DB.Blocks.Memory_IO;

