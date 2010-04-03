-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

package body DB.Blocks.Local_IO is
 
   function To_Valid_Address
     (Position : Low_Level_IO.File_Position_Type)
      return Valid_Address_Type;


   procedure Create
     (ID   : in  String;
      File : out File_Type) is
   begin
      Low_Level_IO.Open(Path      => ID,
                        Open_Kind => Low_Level_IO.Create,
                        File      => File.FD);
   end Create;


   procedure Create_And_Open_Temporary
     (ID   : in  String;
      File : out File_Type) is
   begin
      declare
      begin
         Create(ID, File);
      exception
         when others =>
            Low_Level_IO.Unlink(ID);
            raise;
      end;
      Low_Level_IO.Unlink(ID);
   end Create_And_Open_Temporary;


   procedure Open
     (ID   : in  String;
      File : out File_Type) is
   begin
      Low_Level_IO.Open(Path      => ID,
                        Open_Kind => Low_Level_IO.Read_Write,
                        File      => File.FD);
   end Open;


   procedure Close
     (File : in out File_Type) is
   begin
      Low_Level_IO.Close(File.FD);
   end Close;


   function First
      return Valid_Address_Type is
   begin
      return 1;
   end First;


   function Succ
     (Address : Valid_Address_Type)
      return Valid_Address_Type is
   begin
      return Address + 1;
   end Succ;


   function Image
     (A : in Valid_Address_Type)
      return String is
   begin
      return Valid_Address_Type'Image(A);
   end Image;


   function To_Address
     (Address : Valid_Address_Type)
      return Address_Type is
   begin
      return Address;
   end To_Address;


   function To_Valid_Address
     (Address : Address_Type)
      return Valid_Address_Type is
   begin
      if Address not in Valid_Address_Type then
         raise IO_Error;
      end if;
      return Address;
   end To_Valid_Address;


   function Is_Valid_Address
     (Address : Address_Type)
      return Boolean is
   begin
      return Address in Valid_Address_Type;
   end Is_Valid_Address;


   function To_Valid_Address
     (Position : Low_Level_IO.File_Position_Type)
      return Valid_Address_Type
   is
      pragma Inline (To_Valid_Address);
      use type Low_Level_IO.File_Position_Type;
   begin
      if Position mod Block_Size /= 0 then
         raise IO_Error;
      end if;
      return Valid_Address_Type(Position / Block_Size + 1);
   end To_Valid_Address;


   function To_File_Position
     (Address : Valid_Address_Type)
      return Low_Level_IO.File_Position_Type
   is
      pragma Inline (To_File_Position);
      use type Low_Level_IO.File_Position_Type;
   begin
      if not Address'Valid then
         raise IO_Error;
      end if;
      return Low_Level_IO.File_Position_Type(Address - 1) * Block_Size;
   end To_File_Position;


   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type)
   is
      procedure LL_Read is new Low_Level_IO.PRead(Block_Type);
   begin
      LL_Read(File.FD, To_File_Position(Address), Block);
   end Read;


   procedure Write
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   : in     Block_Type)
   is
      procedure LL_Write is new Low_Level_IO.PWrite(Block_Type);
   begin
      LL_Write(File.FD, To_File_Position(Address), Block);
   end Write;


   procedure Allocate
     (File    : in out File_Type;
      Address :    out Valid_Address_Type)
   is
      Pos : Low_Level_IO.File_Position_Type;
   begin
      Low_Level_IO.Allocate(File.FD, Block_Size, Pos);
      Address := To_Valid_Address(Pos);
   end Allocate;


   procedure Lock
     (File    : in out File_Type;
      Address : in     Address_Type) is
   begin
      Low_Level_IO.Lock(File.FD, To_File_Position(Address), Block_Size);
   end Lock;


   procedure Unlock
     (File    : in out File_Type;
      Address : in     Address_Type) is
   begin
      Low_Level_IO.Unlock(File.FD, To_File_Position(Address), Block_Size);
   end Unlock;


   function FD
     (File : File_Type)
      return Low_Level_IO.File_Descriptor_Type is
   begin
      return File.FD;
   end FD;

end DB.Blocks.Local_IO;

