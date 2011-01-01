-- Abstract:
--
-- see spec
--
-- Copyright 2008--2011 Christoph Schwering

with Ada.Unchecked_Deallocation;

package body DB.Blocks.Memory_IO is

   Max_File_Count : constant := 10;

   type Entry_Type (Name_Length : Positive) is
      record
         Name : String (1 .. Name_Length);
         File : File_Type;
      end record;
   type Entry_Ref_Type is access Entry_Type;
   type Entry_Ref_Array_Type is
      array (Positive range 1 .. Max_File_Count) of Entry_Ref_Type;

   Files : Entry_Ref_Array_Type := (others => null);


   protected body Item_Type is
      procedure Write (Block : in Blocks.Block_Type) is
      begin
         Item_Type.Block := Block;
      end Write;

      function Read return Blocks.Block_Type is
      begin
         return Block;
      end Read;

      procedure Try_Lock (Success : out Boolean) is
      begin
         if Locked then
            Success := False;
         else
            Locked  := True;
            Success := True;
         end if;
      end Try_Lock;

      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      procedure Unlock is
      begin
         Locked := False;
      end Unlock;

      function Is_Locked return Boolean is
      begin
         return Locked;
      end Is_Locked;
   end Item_Type;


   procedure Create (ID : in String; File : out File_Type) is
   begin
      for I in Files'Range loop
         if Files (I) /= null and then Files (I).Name = ID then
            raise IO_Error;
         elsif Files (I) = null then
            File := new File_Object_Type;
            Files (I) := new Entry_Type'(Name_Length => ID'Length,
                                         Name        => ID,
                                         File        => File);
            return;
         end if;
      end loop;
      raise IO_Error;
   end Create;


   procedure Create_And_Open_Temporary (ID : in String; File : out File_Type) is
   begin
      Create (ID, File);
      Unlink (ID);
   end Create_And_Open_Temporary;


   procedure Open (ID : in String; File : out File_Type) is
   begin
      for I in Files'Range loop
         if Files (I) /= null and then Files (I).Name = ID then
            File := Files (I).File;
            return;
         end if;
      end loop;
      raise IO_Error;
   end Open;


   procedure Close (File : in out File_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Entry_Type, Entry_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (File_Object_Type, File_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (Item_Type, Item_Ref_Type);
      procedure Free is new Ada.Unchecked_Deallocation
        (Item_Ref_Array_Type, Item_Ref_Array_Ref_Type);
   begin
      if File = null then
         return;
      end if;

      for I in Files'Range loop
         if Files (I).File /= File then
            for J in Files (I).File.Buffer'Range loop
               Free (Files (I).File.Buffer (J));
            end loop;
            Free (Files (I).File.Buffer);
            Free (Files (I).File);
            Free (Files (I));
            Files (I) := null;
            return;
         end if;
      end loop;
    end Close;


   function Succ (Address : Valid_Address_Type) return Valid_Address_Type is
   begin
      return Address + 1;
   end Succ;


   function Image (A : Valid_Address_Type) return String is
   begin
      return Valid_Address_Type'Image (A);
   end Image;


   function To_Address (Address : Valid_Address_Type) return Address_Type is
   begin
      return Address;
   end To_Address;


   function To_Valid_Address
     (Address : Address_Type)
      return Valid_Address_Type is
   begin
      return Address;
   end To_Valid_Address;


   function Is_Valid_Address
     (Address : Address_Type)
      return Boolean is
   begin
      return Address /= Invalid_Address;
   end Is_Valid_Address;


   procedure Resize_Buffer
     (File : in File_Type) is
   begin
      if File.Maximum > File.Capacity then
         declare
            Capacity : constant Address_Type :=
               File.Maximum * 4 / 3 + 1;
            Buffer   : constant Item_Ref_Array_Ref_Type :=
               new Item_Ref_Array_Type (1 .. Capacity);
         begin
            if File.Buffer /= null then
               Buffer (File.Buffer'Range) := File.Buffer (File.Buffer'Range);
            end if;
            for I in File.Capacity + 1 .. Capacity loop
               Buffer (I) := new Item_Type;
            end loop;
            File.Capacity := Capacity;
            File.Buffer   := Buffer;
         end;
      end if;
   end Resize_Buffer;


   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type)
   is
      use type Blocks.Block_Type;
   begin
      pragma Assert (Address in File.Buffer'Range);
      Block := File.Buffer (Address).Read;
   end Read;


   procedure Write
     (File           : in out File_Type;
      Address        : in     Valid_Address_Type;
      Block          : in     Block_Type;
      Cache_Priority : in     Natural := Natural'First)
   is
      pragma Unreferenced (Cache_Priority);
      use type Blocks.Block_Type;
   begin
      if Address > File.Maximum then -- not thread-safe (that's intended)
         declare
         begin
            Locks.Mutexes.Lock (File.Mutex);
            File.Maximum := Address;
            Resize_Buffer (File);
            Locks.Mutexes.Unlock (File.Mutex);
         exception
            when others =>
               Locks.Mutexes.Unlock (File.Mutex);
               raise;
         end;
      else
         if not File.Buffer (Address).Is_Locked then
            raise IO_Error;
         end if;
      end if;
      pragma Assert (Address in File.Buffer'Range);
      File.Buffer (Address).Write (Block);
      pragma Assert (File.Buffer  (Address).Read = Block);
   end Write;


   procedure Write_New_Block
     (File           : in out File_Type;
      Address        :    out Valid_Address_Type;
      Block          : in     Block_Type;
      Cache_Priority : in     Natural := Natural'First)
   is
      pragma Unreferenced (Cache_Priority);
      use type Blocks.Block_Type;
   begin
      Locks.Mutexes.Lock (File.Mutex);
      File.Maximum := File.Maximum + 1;
      Address := File.Maximum;
      Resize_Buffer (File);
      pragma Assert (Address in File.Buffer'Range);
      File.Buffer (Address).Write (Block);
      pragma Assert (File.Buffer (Address).Read = Block);
      Locks.Mutexes.Unlock (File.Mutex);
   exception
      when others =>
         Locks.Mutexes.Unlock (File.Mutex);
         raise;
   end Write_New_Block;


   procedure Try_Lock
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Timeout : in     Duration := 0.0;
      Success :    out Boolean) is
   begin
      if Timeout = 0.0 then
         File.Buffer (Address).Try_Lock (Success);
      else
         select
            File.Buffer (Address).Lock;
            Success := True;
         or
            delay Timeout;
            Success := False;
         end select;
      end if;
   end Try_Lock;


   procedure Lock (File : in out File_Type; Address : in Valid_Address_Type) is
   begin
      File.Buffer (Address).Lock;
   end Lock;


   procedure Unlock
     (File : in out File_Type;
      Address : in Valid_Address_Type) is
   begin
      File.Buffer (Address).Unlock;
   end Unlock;

end DB.Blocks.Memory_IO;

