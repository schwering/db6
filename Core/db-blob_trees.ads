-- Abstract:
--
-- Many instances of Gen_Blob_Trees with various IO implementations.
--
-- Copyright 2008, 2009 Christoph Schwering

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
with DB.IO.Blocks.Gen_System_Locking_IO;

with DB.Gen_Blob_Trees;

with DB.Types.Keys;
with DB.Types.Values;
pragma Warnings (Off);
with DB.Types.Values.Bounded;
with DB.Types.Values.Unbounded;
pragma Warnings (On);

package DB.Blob_Trees is

   package Keys     renames DB.Types.Keys;
   package Values   renames DB.Types.Values.Unbounded;
   package Value_IO renames Values.Parted;

   package Async_IO    renames DB.IO.Blocks.Asynchronous_IO.IO;
   package CFS_IO      renames DB.IO.Blocks.CFS_IO.IO;
   package Device_IO   renames DB.IO.Blocks.Device_IO.IO;
   package Direct_IO   renames DB.IO.Blocks.Direct_IO.IO;
   package File_IO     renames DB.IO.Blocks.File_IO.IO;
   package File_SL_IO  renames DB.IO.Blocks.File_IO.System_Locking_IO;
   package Memory_IO   renames DB.IO.Blocks.Memory_IO.IO;
   package Cmp_Mem_IO  renames DB.IO.Blocks.Compressed_Memory_IO.IO;

   generic
      with package Block_IO is new DB.IO.Blocks.Gen_IO (<>);
   package Gen_Wrappers is

      package Blob_Trees is new DB.Gen_Blob_Trees
        (Key_Type            => Keys.Key_Type,
         Key_Context_Type    => Keys.Context_Type,
         Read_Key            => Keys.Read,
         Skip_Key            => Keys.Skip,
         Write_Key           => Keys.Write,
         "="                 => Keys."=",
         "<="                => Keys."<=",
         Value_Type          => Values.String_Type,
         Value_Context_Type  => Value_IO.Context_Type,
         Value_Size_Bound    => Value_IO.String_Size_Bound,
         Fold_Value_Contexts => Value_IO.Fold_Contexts,
         Read_Value_Context  => Value_IO.Read_Context,
         Write_Value_Context => Value_IO.Write_Context,
         Read_Part_Of_Value  => Value_IO.Read_Part_Of_String,
         Write_Part_Of_Value => Value_IO.Write_Part_Of_String,
         Is_Context_Free_Serialization => Keys.Is_Context_Free_Serialization,
         Storage_Pool        => Root_Storage_Pool'Class(Global_Pool_Object),
         Block_IO            => Block_IO);

--      package Climb_Caches is new IO.Blocks.Gen_Climb_Caches(Block_IO);
--      package Climb_Cached_Blob_Trees is new DB.Gen_Blob_Trees
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
--      package LRU_Cached_Blob_Trees is new DB.Gen_Blob_Trees
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


   package Async_Blob_Tree_W    is new Gen_Wrappers(Async_IO);
   package Async_Blob_Trees     renames Async_Blob_Tree_W.Blob_Trees;

   package CFS_Blob_Tree_W      is new Gen_Wrappers(CFS_IO);
   package CFS_Blob_Trees       renames CFS_Blob_Tree_W.Blob_Trees;

   package Device_Blob_Tree_W   is new Gen_Wrappers(Device_IO);
   package Device_Blob_Trees    renames Device_Blob_Tree_W.Blob_Trees;

   package Direct_Blob_Tree_W   is new Gen_Wrappers(Direct_IO);
   package Direct_Blob_Trees    renames Direct_Blob_Tree_W.Blob_Trees;

   package File_Blob_Tree_W     is new Gen_Wrappers(File_IO);
   package File_Blob_Trees      renames File_Blob_Tree_W.Blob_Trees;

   package File_SL_Blob_Tree_W  is new Gen_Wrappers(File_SL_IO);
   package File_SL_Blob_Trees   renames File_SL_Blob_Tree_W.Blob_Trees;

   package Memory_Blob_Tree_W   is new Gen_Wrappers(Memory_IO);
   package Memory_Blob_Trees    renames Memory_Blob_Tree_W.Blob_Trees;

   package Cmp_Mem_Blob_Tree_W  is new Gen_Wrappers(Cmp_Mem_IO);
   package Cmp_Mem_Blob_Trees   renames Cmp_Mem_Blob_Tree_W.Blob_Trees;

end DB.Blob_Trees;

