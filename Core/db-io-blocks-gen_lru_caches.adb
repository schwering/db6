with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body DB.IO.Blocks.Gen_LRU_Caches is

   subtype LLI is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
   Hashtable_Size  : LLI := 0;
   Compressed_Size : LLI := 0;
   Count           : LLI := 0;
   Discard_Count   : LLI := 0;

   task Superviser;

   task body Superviser is
   begin
      loop
         Put_Line("Hashtable Size       ="&
                  LLI'Image(Hashtable_Size));
         Put_Line("Compressed Size      ="&
                  LLI'Image(Compressed_Size) &" ="&
                  LLI'Image(Compressed_Size / 2**20) &"MB");
         Put_Line("Uncompressed Size    ="&
                  LLI'Image(Count * LLI(Block_Size)) &" ="&
                  LLI'Image(Count * LLI(Block_Size) / 2**20) &"MB");
         Put_Line("Discard Count        ="&
                  Discard_Count'Img);
         Put_Line("Pool Current Storage ="&
                  LLI'Image(LLI(Pool.Current_Storage_Size) / 2**20) &"MB");
         Put_Line("Pool Maximum Storage ="&
                  LLI'Image(LLI(Pool.Max_Storage_Size) / 2**20) &"MB");
         New_Line;
         delay 30.0;
      end loop;
   end Superviser;


   procedure Free_Entry is
      new Ada.Unchecked_Deallocation(Entry_Type, Entry_Ref_Type);


   procedure Free_Hashtable is
      new Ada.Unchecked_Deallocation(Hashtables.Table_Type,
                                     Hashtables.Table_Ref_Type);


   function Compress (Uncompressed : Block_Type) return Buffer_Type
   is
      Len     : constant Buffer_Size_Type
              := Uncompressed'Length;
      Max_Len : constant Buffer_Size_Type
              := Len + Compression.Deflate.Worst_Deflate_Overhead(Len);
   begin
      return Compression.Deflate.Deflate(Buffer_Type(Uncompressed), Max_Len);
   end Compress;


   function Decompress (Compressed : Buffer_Type) return Block_Type
   is
      Uncompressed : constant Buffer_Type
                   := Compression.Deflate.Inflate(Compressed, Block_Size);
      Block        : Block_Type;
   begin
      Block(1 .. Uncompressed'Length) := Base_Block_Type(Uncompressed);
      return Block;
   end Decompress;


   function Hash (Address : Valid_Address_Type) return Hash_Type
   is
      type Long_Hash_Type is mod 2**Natural'Size;
      for Long_Hash_Type'Size use Natural'Size;
      for Long_Hash_Type'Alignment use Natural'Alignment;
      function Convert is new Ada.Unchecked_Conversion
        (Valid_Address_Type, Long_Hash_Type);
      Long_Hash : constant Long_Hash_Type := Convert(Address);
   begin
      pragma Assert (Address'Size = Long_Hash_Type'Size);
      pragma Assert (Address'Alignment = Long_Hash_Type'Alignment);
      return Hash_Type(Long_Hash);-- mod Hash_Type'Modulus);
   end Hash;


   function Rehash (Hash : Hash_Type) return Hash_Type
   is
      use type Hash_Type;
   begin
      return Hash + 1;
   end Rehash;


   procedure Prepend_Entry
     (File : in out File_Type;
      E    : in out Entry_Ref_Type) is
   begin
      pragma Assert ((File.Head = null) = (File.Tail = null));
      if File.Head /= null then
         E.Prev         := null;
         E.Next         := File.Head;
         File.Head.Prev := E;
         File.Head      := E;
      elsif File.Tail = null then
         E.Prev    := null;
         E.Next    := null;
         File.Head := E;
         File.Tail := E;
      end if;
      pragma Assert ((File.Head = null) = (File.Tail = null));
      pragma Assert ((File.Head = E) = (E.Prev = null));
      pragma Assert ((File.Tail = E) = (E.Next = null));
      Hashtables.Put(File.Table.all, E.Address, E);
      Hashtable_Size := Hashtable_Size + 1;
   end Prepend_Entry;


   procedure Shift_Entry
     (File : in out File_Type;
      E    : in out Entry_Ref_Type)
   is
      Old_Head : Entry_Ref_Type;
   begin
      pragma Assert (File.Head /= null and File.Tail /= null);
      pragma Assert ((File.Head = E) = (E.Prev = null));
      pragma Assert ((File.Tail = E) = (E.Next = null));
      if File.Head = E then
         return;
      end if;
      if File.Head /= null then
         File.Head.Prev := E;
      end if;
      Old_Head  := File.Head;
      File.Head := E;
      if File.Tail = E and E.Prev /= null then
         File.Tail := E.Prev;
      end if;
      if E.Prev /= null then
         E.Prev.Next := E.Next;
      end if;
      if E.Next /= null then
         E.Next.Prev := E.Prev;
      end if;
      E.Prev := null;
      E.Next := Old_Head;
      pragma Assert (File.Head /= null and File.Tail /= null);
      pragma Assert ((File.Head = E) = (E.Prev = null));
      pragma Assert ((File.Tail = E) = (E.Next = null));
   end Shift_Entry;


   procedure Remove_Entry
     (File : in out File_Type;
      E    : in Entry_Ref_Type) is
   begin
      pragma Assert ((File.Head = null) = (File.Tail = null));
      pragma Assert ((File.Head = E) = (E.Prev = null));
      pragma Assert ((File.Tail = E) = (E.Next = null));
      Compressed_Size   := Compressed_Size   - LLI(E.Block'Length);
      Count             := Count             - 1;
      if File.Head = E then
         File.Head := E.Next;
      end if;
      if File.Tail = E then
         File.Tail := E.Prev;
      end if;
      if E.Prev /= null then
         E.Prev.Next := E.Next;
      end if;
      if E.Next /= null then
         E.Next.Prev := E.Prev;
      end if;
      pragma Assert ((File.Head = null) = (File.Tail = null));
      Hashtables.Delete(File.Table.all, E.Address);
      Hashtable_Size := Hashtable_Size - 1;
      declare
         Copy_Of_E : Entry_Ref_Type := E;
      begin
         Free_Entry(Copy_Of_E);
      end;
   end Remove_Entry;


   procedure Discard_Last_Entry
     (File    : in out File_Type;
      Success :    out Boolean) is
   begin
      if File.Tail /= null then
         if File.Tail.Dirty then
            P_IO.Write(File.File, File.Tail.Address,
                       Decompress(File.Tail.Block));
         end if;
         Remove_Entry(File, File.Tail);
         Success := True;
         Discard_Count := Discard_Count + 1;
      else
         Success := False;
      end if;
   end Discard_Last_Entry;


   procedure Create_New_Entry
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   : in     Block_Type;
      E       :    out Entry_Ref_Type)
   is
      Buffer : constant Buffer_Type := Compress(Block);
   begin
      Compressed_Size   := Compressed_Size   + LLI(Buffer'Length);
      Count             := Count             + 1;
      <<Try_Again>>
      declare
         Allocated : Boolean := False;
      begin
         E := new Entry_Type'(Buffer_Size => Buffer'Length,
                              Block       => Buffer,
                              Address     => Address,
                              Dirty       => False,
                              Next        => null,
                              Prev        => null);
         Allocated := True;
         Prepend_Entry(File, E);
         pragma Assert (File.Head = E);
         pragma Assert (E.Prev = null);
         pragma Assert ((File.Tail = E) = (E.Next = null));
      exception
         when Storage_Error | Hashtables.Hash_Table_Error =>
            declare
               Discard_Success : Boolean;
            begin
               if Allocated then
                  Free_Entry(E);
               end if;
               Discard_Last_Entry(File, Discard_Success);
               if Discard_Success then
                  goto Try_Again;
               else
                  raise;
               end if;
            end;
      end;
   end Create_New_Entry;


   procedure Create
     (ID   : in  String;
      File : out File_Type) is
   begin
      Locks.Mutexes.Lock(File.Mutex);
      P_IO.Create(ID, File.File);
      Hashtables.Allocate_Table(Hash_Table_Size, File.Table);
      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Create;


   procedure Open
     (ID   : in  String;
      File : out File_Type) is
   begin
      Locks.Mutexes.Lock(File.Mutex);
      P_IO.Open(ID, File.File);
      Hashtables.Allocate_Table(Hash_Table_Size, File.Table);
      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Open;


   procedure Close
     (File : in out File_Type)
   is
      E : Entry_Ref_Type := File.Head;
      F : Entry_Ref_Type;
   begin
      Locks.Mutexes.Lock(File.Mutex);
      while E /= null loop
         if E.Dirty then
            P_IO.Write(File.File, E.Address, Decompress(E.Block));
         end if;
         F := E.Next;
         Free_Entry(E);
         E := F;
      end loop;
      Free_Hashtable(File.Table);
      P_IO.Close(File.File);
      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Close;


   procedure Set_Block_Count
     (File    : in out File_Type;
      Address : in     Address_Type) is
   begin
      P_IO.Set_Block_Count(File.File, Address);
   end Set_Block_Count;


   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type)
   is
      E     : Entry_Ref_Type;
      Found : Boolean;
   begin
      Locks.Mutexes.Lock(File.Mutex);
      Hashtables.Get(File.Table.all, Address, E, Found);
      if Found then
         Shift_Entry(File, E);
         Block := Decompress(E.Block);
      else
         P_IO.Read(File.File, Address, Block);
         Create_New_Entry(File, Address, Block, E);
      end if;
      declare
         use type Valid_Address_Type;
      begin
         File.Cur_Address := Succ(Address);
         if File.Last_Address < File.Cur_Address then
            File.Last_Address := File.Cur_Address;
         end if;
      end;
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
   is
      E     : Entry_Ref_Type;
      Found : Boolean;
   begin
      Locks.Mutexes.Lock(File.Mutex);
      Hashtables.Get(File.Table.all, Address, E, Found);
      if Found then
         Remove_Entry(File, E);
      end if;
      Create_New_Entry(File, Address, Block, E);
      E.Dirty := True;
      --P_IO.Write(File.File, E.Address, Decompress(E.Block));
      --E.Dirty := False;
      declare
         use type Valid_Address_Type;
      begin
         File.Cur_Address := Succ(Address);
         if File.Last_Address < File.Cur_Address then
            File.Last_Address := File.Cur_Address;
         end if;
      end;
      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Write;


   procedure Seek_New
     (File    : in out File_Type;
      Address :    out Valid_Address_Type)
   is
      use type Valid_Address_Type;
   begin
      Locks.Mutexes.Lock(File.Mutex);
      Address          := File.Last_Address; 
      File.Cur_Address := Address;
      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Seek_New;


   procedure Acquire_Ticket
     (File   : in out File_Type;
      Ticket :    out Ticket_Type) is
   begin
      P_IO.Acquire_Ticket(File.File, Ticket);
   end Acquire_Ticket;


   procedure Release_Ticket
     (File   : in out File_Type;
      Ticket : in     Ticket_Type) is
   begin
      P_IO.Release_Ticket(File.File, Ticket);
   end Release_Ticket;


   procedure Read_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type) is
   begin
      P_IO.Read_Lock(File.File, Ticket);
   end Read_Lock;


   procedure Write_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type) is
   begin
      P_IO.Write_Lock(File.File, Ticket);
   end Write_Lock;


   procedure Certify_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type) is
   begin
      P_IO.Certify_Lock(File.File, Ticket);
   end Certify_Lock;


   procedure Unlock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type) is
   begin
      P_IO.Unlock(File.File, Ticket);
   end Unlock;

end DB.IO.Blocks.Gen_LRU_Caches;

