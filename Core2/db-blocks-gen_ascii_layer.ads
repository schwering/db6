-- Abstract:
--
-- Puts a sequential ASCII layer ontop of block file IO.
--
-- TODO XXX Note still implement this one?
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Blocks.Gen_IO_Signature;

generic
   with package Block_IO is new Gen_IO_Signature (<>);
package DB.Blocks.Gen_ASCII_Layer is
   pragma Pure;

   type Mode_Type is (Read_Only, Write_Only);

   type File_Type is limited private;

   procedure Create
     (ID    : in  String;
      File  : out File_Type);

   procedure Create_And_Open_Temporary
     (ID    : in  String;
      File  : out File_Type);

   procedure Open
     (ID    : in  String;
      File  : out File_Type);

   procedure Unlink
     (ID : in String);

   procedure Close
     (File : in out File_Type);

   procedure Set_Mode
     (File : in out File_Type;
      Mode : in     Mode_Type);

   procedure Read
     (File : in out File_Type;
      Char :    out Character;
      EOF  :    out Boolean);

   procedure Write
     (File : in out File_Type;
      Char : in     Character);

private
   subtype Count_Type is Natural;

   type File_Type is limited
      record
         Block_File : Block_IO.File_Type;
         Address    : Block_IO.Valid_Address_Type;
         Block      : Block_Type;
         Count      : Count_Type;
         Mode       : Mode_Type;
      end record;

end DB.Blocks.Gen_ASCII_Layer;

