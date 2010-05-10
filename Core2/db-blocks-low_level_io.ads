-- Abstract:
--
-- Thin binding of UNIX IO system calls.
--
-- Design Notes:
--
-- See db-blocks-low_level_io-c.c for C wrapper functions. Most calls are
-- directly system calls.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package DB.Blocks.Low_Level_IO is
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

   procedure Unlink
     (Path : in String);

   procedure Close
     (File : in File_Descriptor_Type);

   generic
      type Item_Type is limited private;
   procedure Read
     (File : in  File_Descriptor_Type;
      Pos  : in  File_Position_Type;
      Item : out Item_Type);

   generic
      type Item_Type is limited private;
   procedure Write
     (File : in File_Descriptor_Type;
      Pos  : in File_Position_Type;
      Item : in Item_Type);

   generic
      type Item_Type is limited private;
   procedure Write_New
     (File : in  File_Descriptor_Type;
      Pos  : out File_Position_Type;
      Item : in  Item_Type);

   procedure Seek_End
     (File : in  File_Descriptor_Type;
      Pos  : out File_Position_Type);

   procedure Lock
     (File   : in File_Descriptor_Type;
      Pos    : in File_Position_Type;
      Length : in Size_Type);

   procedure Unlock
     (File   : in File_Descriptor_Type;
      Pos    : in File_Position_Type;
      Length : in Size_Type);

   function Strerror
      return String;

private
   pragma Inline (Open);
   pragma Inline (Close);
   pragma Inline (Read);
   pragma Inline (Write);
   pragma Inline (Write_New);
   pragma Inline (Lock);
   pragma Inline (Unlock);

end DB.Blocks.Low_Level_IO;

