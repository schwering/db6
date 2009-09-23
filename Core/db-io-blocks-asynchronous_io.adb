with Ada.Unchecked_Deallocation;

with DB.Util.Gen_Hashtables;

package body DB.IO.Blocks.Asynchronous_IO is

   task body IO_Task_Type
   is
      Write_Buffer_Size : constant := 1;
      type Block_Ref_Type is access Block_Type;
      type Hash_Type is mod 2 * Write_Buffer_Size;
      type Key_Type is
         record
            FD      : Low_Level.File_Descriptor_Type;
            Address : Valid_Address_Type;
         end record;
      function Hash (K : Key_Type) return Hash_Type;
      function Rehash (H : Hash_Type) return Hash_Type;
      package HT is new Util.Gen_Hashtables
        (Hash_Type  => Hash_Type,
         Key_Type   => Key_Type,
         Value_Type => Block_Ref_Type,
         Hash       => Hash,
         Rehash     => Rehash);

      function Hash (K : Key_Type) return Hash_Type
      is
         I : constant Integer := Integer(K.FD) * Integer(K.Address);
      begin
         return Hash_Type(I mod Integer(Hash_Type'Last));
      end Hash;

      function Rehash (H : Hash_Type) return Hash_Type
      is begin
         return H + 1;
      end Rehash;

      function To_File_Position
        (Address : Valid_Address_Type)
         return Low_Level.File_Position_Type
      is
         pragma Inline (To_File_Position);
         use type Low_Level.File_Position_Type;
      begin
         if not Address'Valid then
            raise IO_Error;
         end if;
         return Low_Level.File_Position_Type(Address - 1) * Block_Size;
      end To_File_Position;

      procedure LL_Read is new Low_Level.Read(Block_Type);
      procedure LL_Write is new Low_Level.Write(Block_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (Block_Type, Block_Ref_Type);

      Table : HT.Table_Type;
   begin
      loop
         select
               accept Read (FD      : in  Low_Level.File_Descriptor_Type;
                            Address : in  Valid_Address_Type;
                            Block   : out Block_Type) do
                  if HT.Contains(Table, Key_Type'(FD, Address)) then
                     Block := HT.Get(Table, Key_Type'(FD, Address)).all;
                  else
                     Low_Level.Seek(FD, To_File_Position(Address));
                     LL_Read(FD, Block);
                  end if;
               end Read;
         or
             when HT.Size(Table) < Write_Buffer_Size =>
               accept Write (FD      : in Low_Level.File_Descriptor_Type;
                             Address : in Valid_Address_Type;
                             Block   : in Block_Type) do
                  HT.Put(Table, Key_Type'(FD, Address),
                         new Block_Type'(Block));
                  requeue Write_Any;
               end Write;
         or
           when IO_Task_Type.Read'Count = 0 =>
               accept Write_Any do
                  declare
                     Key   : Key_Type;
                     Block : Block_Ref_Type;
                  begin
                     HT.Pop(Table, Key, Block);
                     Low_Level.Seek(Key.FD, To_File_Position(Key.Address));
                     LL_Write(Key.FD, Block.all);
                     Free(Block);
                  end;
               end Write_Any;
         or
            when IO_Task_Type.Write'Count = 0 and IO_Task_Type.Read'Count = 0 =>
               accept Force_Write do
                  for I in 1 .. HT.Size(Table) loop
                     declare
                        Key   : Key_Type;
                        Block : Block_Ref_Type;
                     begin
                        HT.Pop(Table, Key, Block);
                        Low_Level.Seek(Key.FD, To_File_Position(Key.Address));
                        LL_Write(Key.FD, Block.all);
                        Free(Block);
                     end;
                  end loop;
               end Force_Write;
         or
            when IO_Task_Type.Read'Count = 0 and IO_Task_Type.Write'Count = 0
               and IO_Task_Type.Write_Any'Count = 0
               and IO_Task_Type.Force_Write'Count = 0 =>
               accept Stop;
               exit;
         end select;
      end loop;
   end IO_Task_Type;


   function To_Valid_Address
     (Position : Low_Level.File_Position_Type)
      return Valid_Address_Type
   is
      pragma Inline (To_Valid_Address);
      use type Low_Level.File_Position_Type;
   begin
      if Position mod Block_Size /= 0 then
         raise IO_Error;
      end if;
      return Valid_Address_Type(Position / Block_Size) + 1;
   end To_Valid_Address;


   procedure Create
     (ID   : in  String;
      File : out File_Type)
   is
      Last_Position : Low_Level.File_Position_Type;
   begin
      Low_Level.Open(Path      => ID,
                     Open_Kind => Low_Level.Create,
                     File      => File.FD);
      Low_Level.Get_Size(File          => File.FD,
                         Last_Position => Last_Position);
      File.Max_Address_Initialized := True;
      File.Max_Address             := To_Valid_Address(Last_Position);
   exception
      when others =>
         File.IO_Task.Stop;
         raise;
   end Create;


   procedure Open
     (ID   : in  String;
      File : out File_Type)
   is
      Last_Position : Low_Level.File_Position_Type;
   begin
      Low_Level.Open(Path      => ID,
                     Open_Kind => Low_Level.Read_Write,
                     File      => File.FD);
      Low_Level.Seek_End(File.FD);
      Low_Level.Get_Size(File          => File.FD,
                         Last_Position => Last_Position);
      File.Max_Address_Initialized := True;
      File.Max_Address             := To_Valid_Address(Last_Position);
   exception
      when others =>
         File.IO_Task.Stop;
         raise;
   end Open;


   procedure Close
     (File : in out File_Type)
   is begin
      File.IO_Task.Force_Write;
      File.IO_Task.Stop;
      Low_Level.Close(File.FD);
   end Close;


   function First
      return Valid_Address_Type
   is begin
      return 1;
   end First;


   function Succ
     (Address : Valid_Address_Type)
      return Valid_Address_Type
   is begin
      return Address + 1;
   end Succ;


   function Image
     (A : in Valid_Address_Type)
      return String
   is begin
      return Valid_Address_Type'Image(A);
   end Image;


   function To_Address
     (Address : Valid_Address_Type)
      return Address_Type
   is begin
      return Address;
   end To_Address;


   function To_Valid_Address
     (Address : Address_Type)
      return Valid_Address_Type
   is begin
      return Address;
   end To_Valid_Address;


   function Is_Valid_Address
     (Address : Address_Type)
      return Boolean
   is begin
      return Address /= Invalid_Address;
   end Is_Valid_Address;


   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type)
   is begin
      Locks.Mutexes.Lock(File.Mutex);

      if File.Max_Address_Initialized
      and then File.Max_Address < Address then
         raise IO_Error;
      end if;
      File.IO_Task.Read(File.FD, Address, Block);

      Locks.Mutexes.Unlock(File.Mutex);

   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Read;


   procedure Write
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   : in     Block_Type)
   is begin
      Locks.Mutexes.Lock(File.Mutex);

      File.IO_Task.Write(File.FD, Address, Block);
      if not File.Max_Address_Initialized
      or else File.Max_Address < Address then
         File.Max_Address := Address;
      end if;

      Locks.Mutexes.Unlock(File.Mutex);

   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Write;


   procedure Seek_New
     (File    : in out File_Type;
      Address :    out Valid_Address_Type)
   is begin
      Locks.Mutexes.Lock(File.Mutex);

      if not File.Max_Address_Initialized then
         raise IO_Error;
      end if;
      File.Max_Address := File.Max_Address + 1;
      Address := File.Max_Address;

      Locks.Mutexes.Unlock(File.Mutex);

   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Seek_New;


   procedure Acquire_Ticket
     (File   : in out File_Type;
      Ticket :    out Locks.Semaphores.Ticket_Type)
   is begin
      Locks.Semaphores.Acquire_Ticket(File.Semaphore, Ticket);
   end Acquire_Ticket;


   procedure Release_Ticket
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type)
   is begin
      Locks.Semaphores.Release_Ticket(File.Semaphore, Ticket);
   end Release_Ticket;


   procedure Read_Lock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type)
   is begin
      Locks.Semaphores.Read_Lock(File.Semaphore, Ticket);
   end Read_Lock;


   procedure Write_Lock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type)
   is begin
      Locks.Semaphores.Write_Lock(File.Semaphore, Ticket);
   end Write_Lock;


   procedure Certify_Lock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type)
   is begin
      Locks.Semaphores.Certify_Lock(File.Semaphore, Ticket);
   end Certify_Lock;


   procedure Unlock
     (File   : in out File_Type;
      Ticket : in     Locks.Semaphores.Ticket_Type)
   is begin
      Locks.Semaphores.Unlock(File.Semaphore, Ticket);
   end Unlock;

end DB.IO.Blocks.Asynchronous_IO;

