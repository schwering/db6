with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
--with Ada.Text_IO; use Ada.Text_IO;

package body DB.IO.Blocks.Gen_Climb_Caches is

   procedure Create
     (ID   : in  String;
      File : out File_Type) is
   begin
      P_IO.Create(ID, File.File);
      File.Hash_Table := new HT.Table_Type'(HT.New_Table(Buffer_Size));
   end Create;


   procedure Open
     (ID   : in  String;
      File : out File_Type) is
   begin
      P_IO.Open(ID, File.File);
      File.Hash_Table := new HT.Table_Type'(HT.New_Table(Buffer_Size));
   end Open;


   procedure Close
     (File : in out File_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation(Block_Type,
                                                       Block_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation(HT.Table_Type,
                                                       Table_Ref_Type);
   begin
      for I in 1 .. File.Length loop
         if File.Buffer(I).Dirty then
            P_IO.Write(File.File, File.Buffer(I).Address,
                       File.Buffer(I).Block.all);
         end if;
         Free(File.Buffer(I).Block);
      end loop;
      Free(File.Hash_Table);
      P_IO.Close(File.File);
   end Close;


   procedure Set_Block_Count
     (File    : in out File_Type;
      Address : in     Address_Type) is
   begin
      P_IO.Set_Block_Count(File.File, Address);
      if Is_Valid_Address(Address) then
         File.Next_Address := Succ(To_Valid_Address(Address));
      else
         File.Next_Address := First;
      end if;
   end Set_Block_Count;


   procedure Check (File : in out File_Type) is
   begin
      if File.Length /= HT.Size(File.Hash_Table.all) then
         --Put_Line("IO_Error IO_Error IO_Error IO_Error IO_Error ");
         raise IO_Error;
      end if;
      for I in 1 .. File.Length loop
         if not HT.Contains(File.Hash_Table.all, File.Buffer(I).Address) then
            --Put_Line("IO_Error IO_Error IO_Error IO_Error IO_Error ");
            --Put_Line("A ="& Image(File.Buffer(I).Address));
            raise IO_Error;
         end if;
         declare
            Index : Index_Type;
            Found : Boolean;
         begin
            HT.Get(File.Hash_Table.all, File.Buffer(I).Address, Index, Found);
            if not Found then
               --Put_Line("IO_Error IO_Error IO_Error IO_Error IO_Error ");
               raise IO_Error;
            end if;
            if Index /= I then
               --Put_Line("IO_Error IO_Error IO_Error IO_Error IO_Error ");
               raise IO_Error;
            end if;
         end;
         for J in I+1 .. File.Length loop
            if File.Buffer(I).Address = File.Buffer(J).Address then
               --Put_Line(Image(File.Buffer(I).Address) &" twice");
               raise IO_Error;
            end if;
         end loop;
      end loop;
   end Check;

   pragma Unreferenced (Check);

   function Hash1 (A : Valid_Address_Type) return Utils.Hash_Type
   is
      use type Utils.Hash_Type;
      S : constant String := Image(A);
      H : Utils.Hash_Type := Utils.Hash_Type'First;
   begin
      for C in S'Range loop
         for I in 0 .. Utils.Hash_Type'Size / 8 - 1 loop
            H := H xor (Character'Pos(S(C)) * 2**(I * 8));
         end loop;
      end loop;
      return H;
   end Hash1;


   function Hash2 (A : Valid_Address_Type) return Utils.Hash_Type
   is
      use type Utils.Hash_Type;
      AA : constant Valid_Address_Type := A;
      for AA'Alignment use 4;
      type Hashs_Type is
         array (1 .. AA'Size / Utils.Hash_Type'Size) of Utils.Hash_Type;
      pragma Pack (Hashs_Type);
      type Pad_Type is array (1 .. AA'Size - Hashs_Type'Size) of Boolean;
      pragma Pack (Pad_Type);
      type Chunk_Type is
         record
            Hashs : Hashs_Type;
            Pad   : Pad_Type;
         end record;
      pragma Pack (Chunk_Type);
      for Chunk_Type'Alignment use 4;
   begin
      declare
         pragma Warnings (Off);
         function Convert is
            new Ada.Unchecked_Conversion(Source => Valid_Address_Type,
                                         Target => Chunk_Type);
         pragma Warnings (On);
         Chunk : constant Chunk_Type := Convert(AA);
         Hash  : Utils.Hash_Type     := Utils.Hash_Type'First;
      begin
         for I in Chunk.Hashs'Range loop
            if not Chunk.Hashs(I)'Valid then
               goto Unhashable;
            end if;
            Hash := Hash xor Chunk.Hashs(I);
         end loop;
         return Hash;
      end;
<<Unhashable>>
      raise Constraint_Error;
   end Hash2;


   function Hash3 (A : Valid_Address_Type) return Utils.Hash_Type
   is
      pragma Warnings (Off);
      function Convert is
         new Ada.Unchecked_Conversion(Source => Valid_Address_Type,
                                      Target => Utils.Hash_Type);
   begin
      return COnvert(A);
   end Hash3;


   function Hash (A : Valid_Address_Type) return Utils.Hash_Type
   renames Hash1;
   pragma Unreferenced (Hash2);
   pragma Unreferenced (Hash3);

   function Rehash (H : Utils.Hash_Type) return Utils.Hash_Type
   is
      use type Utils.Hash_Type;
   begin
      return H + 1;
   end Rehash;


   procedure Climb
     (File  : in out File_Type;
      Index : in     Index_Type)
   is
      pragma Inline (Climb);
   begin
      if Index > File.Buffer'First then
         declare
            Temp : constant Buffer_Element_Type := File.Buffer(Index);
         begin
            HT.Put(File.Hash_Table.all, File.Buffer(Index-1).Address, Index);
            HT.Put(File.Hash_Table.all, File.Buffer(Index).Address, Index-1);
            File.Buffer(Index)   := File.Buffer(Index-1);
            File.Buffer(Index-1) := Temp;
         end;
      else
         HT.Put(File.Hash_Table.all, File.Buffer(Index).Address, Index);
      end if;
   end Climb;


   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type)
   is
      Index : Index_Type;
      Found : Boolean;
   begin
      Locks.Mutexes.Lock(File.Mutex);
      HT.Get(File.Hash_Table.all, Address, Index, Found);
      if Found then
         if Address /= File.Buffer(Index).Address then
            raise IO_Error;
         end if;
         Block := File.Buffer(Index).Block.all;
         Climb(File, Index);
      elsif File.Length < File.Buffer'Length then
         P_IO.Read(File.File, Address, Block);
         File.Length                  := File.Length + 1;
         Index                        := File.Length;
         File.Buffer(Index).Address   := Address;
         File.Buffer(Index).Block     := new Block_Type;
         File.Buffer(Index).Block.all := Block;
         File.Buffer(Index).Dirty     := False;
         HT.Put(File.Hash_Table.all, Address, Index);
      else
         P_IO.Read(File.File, Address, Block);
         if File.Buffer(File.Length).Dirty then
            P_IO.Write(File.File, File.Buffer(File.Length).Address,
                       File.Buffer(File.Length).Block.all);
         end if;
         HT.Delete(File.Hash_Table.all, File.Buffer(File.Length).Address);
         Index                        := File.Length;
         File.Buffer(Index).Address   := Address;
         File.Buffer(Index).Block.all := Block;
         File.Buffer(Index).Dirty     := False;
         HT.Put(File.Hash_Table.all, Address, Index);
      end if;
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
      Index : Index_Type;
      Found : Boolean;
   begin
      Locks.Mutexes.Lock(File.Mutex);
      HT.Get(File.Hash_Table.all, Address, Index, Found);
      if Address = File.Next_Address then
         File.Next_Address := Succ(File.Next_Address);
      end if;
      if Found then
         if Address /= File.Buffer(Index).Address then
            raise IO_Error;
         end if;
         File.Buffer(Index).Block.all := Block;
         File.Buffer(Index).Dirty     := True;
         Climb(File, Index);
      elsif File.Length < File.Buffer'Length then
         File.Length                  := File.Length + 1;
         Index                        := File.Length;
         File.Buffer(Index).Address   := Address;
         File.Buffer(Index).Block     := new Block_Type;
         File.Buffer(Index).Block.all := Block;
         File.Buffer(Index).Dirty     := True;
         HT.Put(File.Hash_Table.all, Address, Index);
      else
         if File.Buffer(File.Length).Dirty then
            P_IO.Write(File.File, File.Buffer(File.Length).Address,
                       File.Buffer(File.Length).Block.all);
         end if;
         HT.Delete(File.Hash_Table.all, File.Buffer(File.Length).Address);
         Index                        := File.Length;
         File.Buffer(Index).Address   := Address;
         File.Buffer(Index).Block.all := Block;
         File.Buffer(Index).Dirty     := True;
         HT.Put(File.Hash_Table.all, Address, Index);
      end if;
      Locks.Mutexes.Unlock(File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock(File.Mutex);
         raise;
   end Write;


   procedure Seek_New
     (File    : in out File_Type;
      Address :    out Valid_Address_Type) is
   begin
      Address           := File.Next_Address;
      File.Next_Address := Succ(File.Next_Address);
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

end DB.IO.Blocks.Gen_Climb_Caches;

