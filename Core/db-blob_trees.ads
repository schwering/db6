-- Abstract:
--
-- Many instances of Gen_Blob_Trees with various IO implementations.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Pool_Global; use System.Pool_Global;
with System.Storage_Pools; use System.Storage_Pools;

with DB.IO.Blocks.Gen_IO;
with DB.IO.Blocks.Asynchronous_IO;
with DB.IO.Blocks.CFS_IO;
with DB.IO.Blocks.Compressed_Memory_IO;
with DB.IO.Blocks.Device_IO;
with DB.IO.Blocks.Direct_IO;
with DB.IO.Blocks.File_IO;
with DB.IO.Blocks.Memory_IO;
--with DB.IO.Blocks.Gen_Climb_Caches;
--with DB.IO.Blocks.Gen_LRU_Caches;
--with DB.IO.Blocks.Gen_System_Locking_IO;

with DB.Gen_Blob_Trees;
with DB.Gen_Blob_Trees.Gen_Check;

with DB.Types.Keys;
with DB.Types.Values;
pragma Warnings (Off);
with DB.Types.Values.Bounded;
with DB.Types.Values.Unbounded;
pragma Warnings (On);

with DB.Utils.Gen_Integer_Image;
with DB.Utils.Gen_String_Image;

package DB.Blob_Trees is

   package Keys            renames Types.Keys;
   package Values          renames Types.Values.Unbounded;
   package Value_IO        renames Values.Parted;
   package Direct_Value_IO renames Values.Uncompressed;

   package Async_IO    renames IO.Blocks.Asynchronous_IO.IO;
   package CFS_IO      renames IO.Blocks.CFS_IO.IO;
   package Device_IO   renames IO.Blocks.Device_IO.IO;
   package Direct_IO   renames IO.Blocks.Direct_IO.IO;
   package File_IO     renames IO.Blocks.File_IO.IO;
   package File_SL_IO  renames IO.Blocks.File_IO.System_Locking_IO;
   package Memory_IO   renames IO.Blocks.Memory_IO.IO;
   package Cmp_Mem_IO  renames IO.Blocks.Compressed_Memory_IO.IO;

   generic
      with package Block_IO is new IO.Blocks.Gen_IO (<>);
   package Gen_Wrappers is

      function Key_Image     is new Utils.Gen_String_Image(Keys.Key_Type);
      function Value_Image   is new Utils.Gen_String_Image(Values.String_Type);
      function Address_Image is
         new Utils.Gen_Integer_Image(Block_IO.Address_Type);

      package Blob_Trees is new Gen_Blob_Trees
        (Key_Type                => Keys.Key_Type,
         Value_Type              => Values.String_Type,
         Compare                 => Keys.Compare,

         Key_Context_Type        => Keys.Context_Type,
         Key_Size_Bound          => Keys.Size_Bound,
         Read_Key                => Keys.Read,
         Skip_Key                => Keys.Skip,
         Write_Key               => Keys.Write,

         Value_Context_Type      => Direct_Value_IO.Context_Type,
         Value_Size_Bound        => Direct_Value_IO.Size_Bound,
         Read_Value              => Direct_Value_IO.Read,
         Skip_Value              => Direct_Value_IO.Skip,
         Write_Value             => Direct_Value_IO.Write,

         Parted_Value_Context_Type => Value_IO.Context_Type,
         Parted_Value_Size_Bound   => Value_IO.Size_Bound,
         Fold_Value_Contexts       => Value_IO.Fold_Contexts,
         Value_Context_Size_Bound  => Value_IO.Context_Size_Bound,
         Read_Value_Context        => Value_IO.Read_Context,
         Write_Value_Context       => Value_IO.Write_Context,
         Read_Part_Of_Value        => Value_IO.Read_Part_Of_String,
         Write_Part_Of_Value       => Value_IO.Write_Part_Of_String,

         Is_Context_Free_Serialization => Keys.Is_Context_Free_Serialization,
         Storage_Pool            => Root_Storage_Pool'Class(Global_Pool_Object),
         Block_IO                => Block_IO);
      procedure Check is new Blob_Trees.Gen_Check
         (Key_Image, Value_Image);

--      package Climb_Caches is new IO.Blocks.Gen_Climb_Caches(Block_IO);
--      package Climb_Cached_Blob_Trees is new Gen_Blob_Trees
--        (Key_Type           => Keys.Key_Type,
--         Key_Context_Type   => Keys.Context_Type,
--         Read_Key           => Keys.Read,
--         Skip_Key           => Keys.Skip,
--         Write_Key          => Keys.Write,
--         "="                => Keys."=",
--         "<="               => Keys."<=",
--         Value_Type         => Values.String_Type,
--         Value_Context_Type => Value_IO.Context_Type,
--         Read_Value         => Value_IO.Read,
--         Skip_Value         => Value_IO.Skip,
--         Write_Value        => Value_IO.Write,
--         Is_Context_Free_Serialization => 
--                               Keys.Is_Context_Free_Serialization and
--                               Value_IO.Is_Context_Free_Serialization,
--         Storage_Pool       => Root_Storage_Pool'Class(Global_Pool_Object),
--         Block_IO           => Climb_Caches.IO);

--      package LRU_Caches is new IO.Blocks.Gen_LRU_Caches(Block_IO);
--      package LRU_Cached_Blob_Trees is new Gen_Blob_Trees
--        (Key_Type           => Keys.Key_Type,
--         Key_Context_Type   => Keys.Context_Type,
--         Read_Key           => Keys.Read,
--         Skip_Key           => Keys.Skip,
--         Write_Key          => Keys.Write,
--         "="                => Keys."=",
--         "<="               => Keys."<=",
--         Value_Type         => Values.String_Type,
--         Value_Context_Type => Value_IO.Context_Type,
--         Read_Value         => Value_IO.Read,
--         Skip_Value         => Value_IO.Skip,
--         Write_Value        => Value_IO.Write,
--         Is_Context_Free_Serialization => 
--                               Keys.Is_Context_Free_Serialization and
--                               Value_IO.Is_Context_Free_Serialization,
--         Storage_Pool       => Root_Storage_Pool'Class(Global_Pool_Object),
--         Block_IO           => LRU_Caches.IO);

   end Gen_Wrappers;


   -- XXX uncomment (or remove)
   --package Async_Blob_Tree_W    is new Gen_Wrappers(Async_IO);
   --package Async_Blob_Trees     renames Async_Blob_Tree_W.Blob_Trees;
   --procedure Check (T : in out Async_Blob_Trees.Tree_Type)
   --renames Async_Blob_Tree_W.Check;

   --package CFS_Blob_Tree_W      is new Gen_Wrappers(CFS_IO);
   --package CFS_Blob_Trees       renames CFS_Blob_Tree_W.Blob_Trees;
   --procedure Check (T : in out CFS_Blob_Trees.Tree_Type)
   --renames CFS_Blob_Tree_W.Check;

   --package Device_Blob_Tree_W   is new Gen_Wrappers(Device_IO);
   --package Device_Blob_Trees    renames Device_Blob_Tree_W.Blob_Trees;
   --procedure Check (T : in out Device_Blob_Trees.Tree_Type)
   --renames Device_Blob_Tree_W.Check;

   --package Direct_Blob_Tree_W   is new Gen_Wrappers(Direct_IO);
   --package Direct_Blob_Trees    renames Direct_Blob_Tree_W.Blob_Trees;
   --procedure Check (T : in out Direct_Blob_Trees.Tree_Type)
   --renames Direct_Blob_Tree_W.Check;

   --package File_Blob_Tree_W     is new Gen_Wrappers(File_IO);
   --package File_Blob_Trees      renames File_Blob_Tree_W.Blob_Trees;
   --procedure Check (T : in out File_Blob_Trees.Tree_Type)
   --renames File_Blob_Tree_W.Check;

   --package File_SL_Blob_Tree_W  is new Gen_Wrappers(File_SL_IO);
   --package File_SL_Blob_Trees   renames File_SL_Blob_Tree_W.Blob_Trees;
   --procedure Check (T : in out File_SL_Blob_Trees.Tree_Type)
   --renames File_SL_Blob_Tree_W.Check;

   --package Memory_Blob_Tree_W   is new Gen_Wrappers(Memory_IO);
   --package Memory_Blob_Trees    renames Memory_Blob_Tree_W.Blob_Trees;
   --procedure Check (T : in out Memory_Blob_Trees.Tree_Type)
   --renames Memory_Blob_Tree_W.Check;

   --package Cmp_Mem_Blob_Tree_W  is new Gen_Wrappers(Cmp_Mem_IO);
   --package Cmp_Mem_Blob_Trees   renames Cmp_Mem_Blob_Tree_W.Blob_Trees;
   --procedure Check (T : in out Cmp_Mem_Blob_Trees.Tree_Type)
   --renames Cmp_Mem_Blob_Tree_W.Check;

end DB.Blob_Trees;

