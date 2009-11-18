-- Abstract:
--
-- Thin binding of UNIX IO system calls.
--
-- Design Notes:
--
-- See db-io-low_level-c.c for C wrapper functions. Most calls are directly
-- system calls.
--
-- Copyright 2008, 2009 Christoph Schwering

package DB.IO.Low_Level is
   pragma Preelaborate;

   type Open_Kind_Type is (Create, Read_Only, Read_Write);
   type File_Descriptor_Type is range -1 .. Integer'Last;
   type Lock_Kind_Type is (Exclusive, Shared);
   type Seek_Kind_Type is (From_Beginning, From_Position, From_End);
   type File_Position_Type is mod 2**64;
   subtype Size_Type is Natural;
   subtype Signed_Size_Type is Integer;

   procedure Open
     (Path      : in  String;
      Open_Kind : in  Open_Kind_Type := Read_Write;
      File      : out File_Descriptor_Type);

   procedure Open_Direct
     (Path      : in  String;
      Open_Kind : in  Open_Kind_Type := Read_Write;
      File      : out File_Descriptor_Type);

   procedure Close
     (File : in File_Descriptor_Type);

   procedure Lock
     (File     : in  File_Descriptor_Type;
      Kind     : in  Lock_Kind_Type;
      Blocking : in  Boolean;
      Success  : out Boolean);

   procedure Unlock
     (File     : in  File_Descriptor_Type;
      Success  : out Boolean);

   procedure Get_Size
     (File          : in  File_Descriptor_Type;
      Last_Position : out File_Position_Type);

   procedure Seek
     (File    : in File_Descriptor_Type;
      Pos     : in File_Position_Type;
      Kind    : in Seek_Kind_Type := From_Beginning);

   procedure Seek
     (File    : in  File_Descriptor_Type;
      Pos     : in  File_Position_Type;
      Kind    : in  Seek_Kind_Type := From_Beginning;
      New_Pos : out File_Position_Type);

   procedure Seek_End
     (File    : in  File_Descriptor_Type;
      New_Pos : out File_Position_Type);

   function Current_File_Position
     (File : File_Descriptor_Type)
      return File_Position_Type;

   generic
      type Item_Type is limited private;
   procedure Read
     (File : in  File_Descriptor_Type;
      Item : out Item_Type);

   generic
      type Item_Type is limited private;
   procedure PRead
     (File : in  File_Descriptor_Type;
      Pos  : in  File_Position_Type;
      Item : out Item_Type);

   generic
      type Item_Type is limited private;
   procedure Write
     (File : in File_Descriptor_Type;
      Item : in Item_Type);

   generic
      type Item_Type is limited private;
   procedure PWrite
     (File : in File_Descriptor_Type;
      Pos  : in File_Position_Type;
      Item : in Item_Type);

   generic
      type Item_Type is limited private;
   procedure Read_Direct
     (File : in  File_Descriptor_Type;
      Item : out Item_Type);

   generic
      type Item_Type is limited private;
   procedure Write_Direct
     (File : in File_Descriptor_Type;
      Item : in Item_Type);

   function Strerror
      return String;

private
   pragma Inline (Open);
   pragma Inline (Close);
   pragma Inline (Get_Size);
   pragma Inline (Seek);
   pragma Inline (Seek_End);
   pragma Inline (Current_File_Position);
   pragma Inline (Read);
   pragma Inline (Write);
   pragma Inline (Lock);
   pragma Inline (Unlock);

end DB.IO.Low_Level;

