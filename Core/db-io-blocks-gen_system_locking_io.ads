-- Abstract:
--
-- Wrapper for a normal IO implementation that works with system file
-- descriptors that adds file locking on operating system level.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.IO.Blocks.Gen_IO;
with DB.IO.Low_Level;

generic
   with package P_IO is new Gen_IO (<>);
   with function FD (File : P_IO.File_Type)
           return Low_Level.File_Descriptor_Type;
package DB.IO.Blocks.Gen_System_Locking_IO is
   pragma Preelaborate;

   type File_Type is
      record
         File : P_IO.File_Type;
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

   procedure Close
     (File : in out File_Type);

   procedure Set_Block_Count
     (File    : in out File_Type;
      Address : in     P_IO.Address_Type);

   procedure Read
     (File    : in out File_Type;
      Address : in     P_IO.Valid_Address_Type;
      Block   :    out Block_Type);

   procedure Write
     (File    : in out File_Type;
      Address : in     P_IO.Valid_Address_Type;
      Block   : in     Block_Type);

   procedure Seek_New
     (File    : in out File_Type;
      Address :    out P_IO.Valid_Address_Type);

   procedure Acquire_Ticket
     (File   : in out File_Type;
      Ticket :    out P_IO.Ticket_Type);

   procedure Release_Ticket
     (File   : in out File_Type;
      Ticket : in     P_IO.Ticket_Type);

   procedure Read_Lock
     (File   : in out File_Type;
      Ticket : in     P_IO.Ticket_Type);

   procedure Write_Lock
     (File   : in out File_Type;
      Ticket : in     P_IO.Ticket_Type);

   procedure Certify_Lock
     (File   : in out File_Type;
      Ticket : in     P_IO.Ticket_Type);

   procedure Unlock
     (File   : in out File_Type;
      Ticket : in     P_IO.Ticket_Type);


   package IO is new Gen_IO
     (File_Type                  => File_Type,
      Ticket_Type                => P_IO.Ticket_Type,
      Address_Type               => P_IO.Address_Type,
      Needs_Explicit_Block_Count => P_IO.Needs_Explicit_Block_Count,
      Valid_Address_Type         => P_IO.Valid_Address_Type,
      Invalid_Address            => P_IO.Invalid_Address,
      Create                     => Create,
      Create_And_Open_Temporary  => Create_And_Open_Temporary,
      Open                       => Open,
      Set_Block_Count            => Set_Block_Count,
      Close                      => Close,
      Succ                       => P_IO.Succ,
      "<"                        => P_IO."<",
      "="                        => P_IO."=",
      Image                      => P_IO.Image,
      First                      => P_IO.First,
      To_Address                 => P_IO.To_Address,
      To_Valid_Address           => P_IO.To_Valid_Address,
      Is_Valid_Address           => P_IO.Is_Valid_Address,
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
   pragma Inline (Create);
   pragma Inline (Open);
   pragma Inline (Set_Block_Count);
   pragma Inline (Close);
   pragma Inline (Read);
   pragma Inline (Write);
   pragma Inline (Acquire_Ticket);
   pragma Inline (Release_Ticket);
   pragma Inline (Read_Lock);
   pragma Inline (Write_Lock);
   pragma Inline (Certify_Lock);
   pragma Inline (Unlock);

end DB.IO.Blocks.Gen_System_Locking_IO;

