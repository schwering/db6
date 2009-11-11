package body DB.IO.Blocks.Gen_System_Locking_IO is

   procedure Create
     (ID   : in  String;
      File : out File_Type) is
   begin
      P_IO.Create(ID, File.File);
   end Create;


   procedure Open
     (ID   : in  String;
      File : out File_Type) is
   begin
      P_IO.Open(ID, File.File);
   end Open;


   procedure Close
     (File : in out File_Type) is
   begin
      P_IO.Close(File.File);
   end Close;


   procedure Set_Block_Count
     (File    : in out File_Type;
      Address : in     P_IO.Address_Type) is
   begin
      P_IO.Set_Block_Count(File.File, Address);
   end Set_Block_Count;


   procedure Read
     (File    : in out File_Type;
      Address : in     P_IO.Valid_Address_Type;
      Block   :    out Block_Type) is
   begin
      P_IO.Read(File.File, Address, Block);
   end Read;


   procedure Write
     (File    : in out File_Type;
      Address : in     P_IO.Valid_Address_Type;
      Block   : in     Block_Type) is
   begin
      P_IO.Write(File.File, Address, Block);
   end Write;


   procedure Seek_New
     (File    : in out File_Type;
      Address :    out P_IO.Valid_Address_Type) is
   begin
      P_IO.Seek_New(File.File, Address);
   end Seek_New;


   procedure Acquire_Ticket
     (File   : in out File_Type;
      Ticket :    out P_IO.Ticket_Type) is
   begin
      P_IO.Acquire_Ticket(File.File, Ticket);
   end Acquire_Ticket;


   procedure Release_Ticket
     (File   : in out File_Type;
      Ticket : in     P_IO.Ticket_Type) is
   begin
      P_IO.Release_Ticket(File.File, Ticket);
   end Release_Ticket;


   procedure Read_Lock
     (File   : in out File_Type;
      Ticket : in     P_IO.Ticket_Type)
   is
      Successful : Boolean;
   begin
      P_IO.Read_Lock(File.File, Ticket);
      -- Simply acquiring a shared lock here does not work, because this
      -- lock might downgrade an exclusive lock that exists due to a Write_Lock
      -- since Read_Locks are allowed during a Write_Lock is hold!
      Low_Level.Lock(FD(File.File), Low_Level.Exclusive, True, Successful);
      if not Successful then
         raise IO_Error;
      end if;
   end Read_Lock;


   procedure Write_Lock
     (File   : in out File_Type;
      Ticket : in     P_IO.Ticket_Type)
   is
      Successful : Boolean;
   begin
      P_IO.Write_Lock(File.File, Ticket);
      Low_Level.Lock(FD(File.File), Low_Level.Exclusive, True, Successful);
      if not Successful then
         raise IO_Error;
      end if;
   end Write_Lock;


   procedure Certify_Lock
     (File   : in out File_Type;
      Ticket : in     P_IO.Ticket_Type)
   is
      Successful : Boolean;
   begin
      Low_Level.Lock(FD(File.File), Low_Level.Exclusive, True, Successful);
      if not Successful then
         raise IO_Error;
      end if;
      P_IO.Certify_Lock(File.File, Ticket);
   end Certify_Lock;


   procedure Unlock
     (File   : in out File_Type;
      Ticket : in     P_IO.Ticket_Type)
   is
      Successful : Boolean;
   begin
      P_IO.Unlock(File.File, Ticket);
      Low_Level.Unlock(FD(File.File), Successful);
      if not Successful then
         raise IO_Error;
      end if;
   end Unlock;

end DB.IO.Blocks.Gen_System_Locking_IO;

