with System.Storage_Pools;

with DB.IO.Blocks.Gen_IO;

generic
   with package Block_IO is new Gen_IO (<>);
   type Item_Type (<>) is private;
   with function To_Block (I : Item_Type) return Block_Type;
   with function From_Block (B : Block_Type) return Item_Type;
   Item_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
   Node_Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
package DB.IO.Blocks.Gen_Buffers is
   pragma Preelaborate;

   type Buffer_Type is private;

   type Item_Ref_Type is access Item_Type;
   for Item_Ref_Type'Storage_Pool use Item_Storage_Pool;
   pragma Controlled (Item_Ref_Type);

   type Item_Constant_Ref_Type is access constant Item_Type;
   for Item_Constant_Ref_Type'Storage_Size use 0;
   pragma Controlled (Item_Constant_Ref_Type);

   function New_Buffer
      return Buffer_Type;

   procedure Free
     (Buffer : in out Buffer_Type);

   procedure Commit
     (File   : in out Block_IO.File_Type;
      Buffer : in     Buffer_Type);

   procedure Seek_New
     (File    : in out Block_IO.File_Type;
      Buffer  : in out Buffer_Type;
      Address :    out Block_IO.Valid_Address_Type);

   function "<="
     (A, B : Block_IO.Valid_Address_Type)
      return Boolean;

   generic
      File   : in out Block_IO.File_Type;
      Buffer : in out Buffer_Type;
   function Gen_Read
     (Address : Block_IO.Valid_Address_Type)
      return Item_Type;

   procedure Read
     (File    : in out Block_IO.File_Type;
      Buffer  : in out Buffer_Type;
      Address : in     Block_IO.Valid_Address_Type;
      Item    :    out Item_Type);

   procedure Read
     (File     : in out Block_IO.File_Type;
      Buffer   : in out Buffer_Type;
      Address  : in     Block_IO.Valid_Address_Type;
      Item_Ref :    out Item_Constant_Ref_Type);

   procedure Write
     (File    : in out Block_IO.File_Type;
      Buffer  : in out Buffer_Type;
      Address : in     Block_IO.Valid_Address_Type;
      Item    : in     Item_Type);

private
   type Entry_Type;
   type Entry_Ref_Type is access Entry_Type;
   for Entry_Ref_Type'Storage_Pool use Node_Storage_Pool;

   type Entry_Type is
      record
         Next    : Entry_Ref_Type;
         Address : Block_IO.Valid_Address_Type;
         Item    : Item_Ref_Type;
         Changed : Boolean := False;
      end record;

   type Buffer_Type is
      record
         Head                    : Entry_Ref_Type := null;
         New_Address_Initialized : Boolean := False;
         New_Address             : Block_IO.Valid_Address_Type;
      end record;

end DB.IO.Blocks.Gen_Buffers;

