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
     (P_File_Type                  => File_Type,
      P_Ticket_Type                => P_IO.Ticket_Type,
      P_Address_Type               => P_IO.Address_Type,
      P_Needs_Explicit_Block_Count => P_IO.Needs_Explicit_Block_Count,
      P_Valid_Address_Type         => P_IO.Valid_Address_Type,
      P_Invalid_Address            => P_IO.Invalid_Address,
      P_Create                     => Create,
      P_Open                       => Open,
      P_Set_Block_Count            => Set_Block_Count,
      P_Close                      => Close,
      P_Succ                       => P_IO.Succ,
      P_Is_Less                    => P_IO."<",
      P_Is_Equal                   => P_IO."=",
      P_Image                      => P_IO.Image,
      P_First                      => P_IO.First,
      P_To_Address                 => P_IO.To_Address,
      P_To_Valid_Address           => P_IO.To_Valid_Address,
      P_Is_Valid_Address           => P_IO.Is_Valid_Address,
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

