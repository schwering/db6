-- Abstract:
--
-- An implementation of the LRU cache strategy.
--
-- Design Notes:
--
-- A linked list of blocks is used to order the blocks in cache and a hashtable
-- is used for fast access to them.
-- The cache uses a storage pool with a certain size. When no new memory from
-- this pool can be allocated, an cached block is discarded.
--
-- Copyright 2008, 2009 Christoph Schwering

with System.Storage_Pools;

with DB.Compression.Deflate;
with DB.IO.Blocks.Gen_IO;
with DB.Locks.Mutexes;
with DB.Utils.Gen_Hashtables;
with DB.Utils.Bounded_Pools;

generic
   with package P_IO is new Gen_IO (<>);
package DB.IO.Blocks.Gen_LRU_Caches is
   pragma Elaborate_Body;

   subtype Address_Type is P_IO.Address_Type;
   subtype Valid_Address_Type is P_IO.Valid_Address_Type;
   subtype Ticket_Type is P_IO.Ticket_Type;

   Invalid_Address : constant Address_Type := P_IO.Invalid_Address;
   Needs_Explicit_Block_Count : constant Boolean
      := P_IO.Needs_Explicit_Block_Count;

   Mega_Byte       : constant := 2**20;
   Pool_Size       : constant := 2**10 * Mega_Byte;
   Pool            : Utils.Bounded_Pools.Bounded_No_Reclaim_Pool(Pool_Size);
   Hash_Table_Size : constant := Pool_Size / (Block_Size / 20);

   subtype Buffer_Type is Compression.Deflate.Buffer_Type;
   subtype Buffer_Size_Type is Compression.Deflate.Size_Type;

   subtype Hash_Type is Utils.Hash_Type;
   function Hash (Address : Valid_Address_Type) return Hash_Type;
   pragma Inline (Hash);
   function Rehash (Hash : Hash_Type) return Hash_Type;
   pragma Inline (Rehash);

   type Entry_Type;
   type Entry_Ref_Type is access Entry_Type;
   for Entry_Ref_Type'Storage_Pool use Pool;
   type Entry_Type (Buffer_Size : Buffer_Size_Type) is
      record
         Address : Valid_Address_Type;
         Block   : Buffer_Type(1 .. Buffer_Size);
         Dirty   : Boolean        := False;
         Next    : Entry_Ref_Type := null;
         Prev    : Entry_Ref_Type := null;
      end record;

   package Hashtables is new Utils.Gen_Hashtables
     (Key_Type     => Valid_Address_Type,
      Value_Type   => Entry_Ref_Type,
      Hash         => Hash,
      Rehash       => Rehash,
      "="          => P_IO."=",
      Storage_Pool => System.Storage_Pools.Root_Storage_Pool'Class(Pool));

   type File_Type is limited
      record
         File         : P_IO.File_Type;
         Mutex        : Locks.Mutexes.Mutex_Type;
         Table        : Hashtables.Table_Ref_Type;
         Head         : Entry_Ref_Type;
         Tail         : Entry_Ref_Type;
         Cur_Address  : Valid_Address_Type := P_IO.First;
         Last_Address : Valid_Address_Type := P_IO.First;
         Last_Written : Boolean            := False;
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
      Address : in     Address_Type);

   function First
      return Valid_Address_Type
   renames P_IO.First;

   function Succ
     (Address : Valid_Address_Type)
      return Valid_Address_Type
   renames P_IO.Succ;

   function "<"
     (A, B : Valid_Address_Type)
      return Boolean
   renames P_IO."<";

   function "="
     (A, B : Valid_Address_Type)
      return Boolean
   renames P_IO."=";

   function Image
     (A : in Valid_Address_Type)
      return String
   renames P_IO.Image;

   function To_Address
     (Address : Valid_Address_Type)
      return Address_Type
   renames P_IO.To_Address;

   function To_Valid_Address
     (Address : Address_Type)
      return Valid_Address_Type
   renames P_IO.To_Valid_Address;

   function Is_Valid_Address
     (Address : Address_Type)
      return Boolean
   renames P_IO.Is_Valid_Address;

   procedure Read
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   :    out Block_Type);

   procedure Write
     (File    : in out File_Type;
      Address : in     Valid_Address_Type;
      Block   : in     Block_Type);

   procedure Seek_New
     (File    : in out File_Type;
      Address :    out Valid_Address_Type);

   procedure Acquire_Ticket
     (File   : in out File_Type;
      Ticket :    out Ticket_Type);

   procedure Release_Ticket
     (File   : in out File_Type;
      Ticket : in     Ticket_Type);

   procedure Read_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type);

   procedure Write_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type);

   procedure Certify_Lock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type);

   procedure Unlock
     (File   : in out File_Type;
      Ticket : in     Ticket_Type);


   package IO is new Gen_IO
     (File_Type                  => File_Type,
      Ticket_Type                => Ticket_Type,
      Address_Type               => Address_Type,
      Needs_Explicit_Block_Count => Needs_Explicit_Block_Count,
      Valid_Address_Type         => Valid_Address_Type,
      Invalid_Address            => Invalid_Address,
      Create                     => Create,
      Open                       => Open,
      Set_Block_Count            => Set_Block_Count,
      Close                      => Close,
      Succ                       => Succ,
      "<"                        => "<",
      "="                        => "=",
      Image                      => Image,
      First                      => First,
      To_Address                 => To_Address,
      To_Valid_Address           => To_Valid_Address,
      Is_Valid_Address           => Is_Valid_Address,
      Read                       => Read,
      Write                      => Write,
      Seek_New                   => Seek_New,
      Acquire_Ticket             => Acquire_Ticket,
      Release_Ticket             => Release_Ticket,
      Read_Lock                  => Read_Lock,
      Write_Lock                 => Write_Lock,
      Certify_Lock               => Certify_Lock,
      Unlock                     => Unlock);

end DB.IO.Blocks.Gen_LRU_Caches;

