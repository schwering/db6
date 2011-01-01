-- Abstract:
--
-- Main memory IO implementation.
--
-- Copyright 2008--2011 Christoph Schwering

with DB.Blocks.Gen_IO_Signature;
private with DB.Locks.Mutexes;

package DB.Blocks.Memory_IO is
   pragma Preelaborate;

   type Address_Type is new Natural;
   subtype Valid_Address_Type is Address_Type range 1 .. Address_Type'Last;
   type File_Object_Type is limited private;
   type File_Type is access File_Object_Type;
   pragma Controlled (File_Type);

   Invalid_Address : constant Address_Type := 0;
   First_Address   : constant Address_Type := 1;

   procedure Create (ID : in String; File : out File_Type);

   procedure Create_And_Open_Temporary (ID : in String; File : out File_Type);

   procedure Open (ID : in String; File : out File_Type);

   procedure Close (File : in out File_Type);

   procedure Unlink (ID : in String) is null;

   function Succ (Address : Valid_Address_Type) return Valid_Address_Type;

   function Image (A : Valid_Address_Type) return String;

   function To_Address (Address : Valid_Address_Type) return Address_Type;

   function To_Valid_Address (Address : Address_Type) return Valid_Address_Type;

   function Is_Valid_Address (Address : Address_Type) return Boolean;

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
      Timeout : in     Duration := 0.0;
      Success :    out Boolean);

   procedure Lock (File : in out File_Type; Address : in Valid_Address_Type);

   procedure Unlock (File : in out File_Type; Address : in Valid_Address_Type);


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
      Try_Lock                   => Try_Lock,
      Lock                       => Lock,
      Unlock                     => Unlock);

private
   -- We use a protected type to ensure that during a Read no Write can destroy
   -- the block's integrity.
   -- Since we need the protected type for this purpose, we can also merge the
   -- mutex into this protected type, of course.
   protected type Item_Type is
      procedure Write (Block : in Blocks.Block_Type);
      function Read return Blocks.Block_Type;
      procedure Try_Lock (Success : out Boolean);
      entry Lock;
      procedure Unlock;
      function Is_Locked return Boolean;

   private
      Block  : Blocks.Block_Type;
      Locked : Boolean := False;
   end Item_Type;

   type Item_Ref_Type is access Item_Type;
   pragma Controlled (Item_Ref_Type);

   type Item_Ref_Array_Type is
      array (Valid_Address_Type range <>) of Item_Ref_Type;

   type Item_Ref_Array_Ref_Type is access Item_Ref_Array_Type;
   pragma Controlled (Item_Ref_Array_Ref_Type);

   type File_Object_Type is limited
      record
         Buffer   : Item_Ref_Array_Ref_Type := null;
         Capacity : Address_Type := 0;
         Maximum  : Address_Type := 0;
         Mutex    : Locks.Mutexes.Mutex_Type; -- for atom. of seek+read/write
      end record;

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

end DB.Blocks.Memory_IO;

