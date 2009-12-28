-- Abstract:
--
-- Many instances of Gen_BTrees with various IO implementations.
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

with DB.Gen_BTrees;
with DB.Gen_BTrees.Gen_Stats;
with DB.Gen_BTrees.Gen_Check;

with DB.Types.Keys;
with DB.Types.Values;
pragma Warnings (Off);
with DB.Types.Values.Bounded;
with DB.Types.Values.Unbounded;
pragma Warnings (On);

with DB.Utils.Gen_Integer_Image;
with DB.Utils.Gen_String_Image;

package DB.BTrees is

   package Keys     renames Types.Keys;
   package Values   renames Types.Values.Bounded;
   package Value_IO renames Values.Uncompressed;

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

      package BTrees is new Gen_BTrees
        (Key_Type           => Keys.Key_Type,
         Key_Context_Type   => Keys.Context_Type,
         Key_Size_Bound     => Keys.Size_Bound,
         Read_Key           => Keys.Read,
         Skip_Key           => Keys.Skip,
         Write_Key          => Keys.Write,
         "="                => Keys."=",
         "<="               => Keys."<=",
         Value_Type         => Values.String_Type,
         Value_Context_Type => Value_IO.Context_Type,
         Value_Size_Bound   => Value_IO.Size_Bound,
         Read_Value         => Value_IO.Read,
         Skip_Value         => Value_IO.Skip,
         Write_Value        => Value_IO.Write,
         Is_Context_Free_Serialization => 
                               Keys.Is_Context_Free_Serialization and
                               Value_IO.Is_Context_Free_Serialization,
         Storage_Pool       => Root_Storage_Pool'Class(Global_Pool_Object),
         Block_IO           => Block_IO);
      procedure Stats is new BTrees.Gen_Stats;
      procedure Check is new BTrees.Gen_Check
         (Key_Image, Value_Image, Address_Image);

--      package Climb_Caches is new IO.Blocks.Gen_Climb_Caches(Block_IO);
--      package Climb_Cached_BTrees is new Gen_BTrees
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
--      procedure Stats is new Climb_Cached_BTrees.Gen_Stats;
--      procedure Check is new Climb_Cached_BTrees.Gen_Check
--         (Key_Image, Value_Image, Address_Image);

--      package LRU_Caches is new IO.Blocks.Gen_LRU_Caches(Block_IO);
--      package LRU_Cached_BTrees is new Gen_BTrees
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
--      procedure Stats is new LRU_Cached_BTrees.Gen_Stats;
--      procedure Check is new LRU_Cached_BTrees.Gen_Check
--         (Key_Image, Value_Image, Address_Image);

   end Gen_Wrappers;


   package Async_BTree_W    is new Gen_Wrappers(Async_IO);
   package Async_BTrees     renames Async_BTree_W.BTrees;
   procedure Check (T : in out Async_BTrees.Tree_Type)
   renames Async_BTree_W.Check;
   procedure Stats
     (Tree                   : in out Async_BTrees.Tree_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer)
   renames Async_BTree_W.Stats;

   package CFS_BTree_W      is new Gen_Wrappers(CFS_IO);
   package CFS_BTrees       renames CFS_BTree_W.BTrees;
   procedure Check (T : in out CFS_BTrees.Tree_Type)
   renames CFS_BTree_W.Check;
   procedure Stats
     (Tree                   : in out CFS_BTrees.Tree_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer)
   renames CFS_BTree_W.Stats;

   package Device_BTree_W   is new Gen_Wrappers(Device_IO);
   package Device_BTrees    renames Device_BTree_W.BTrees;
   procedure Check (T : in out Device_BTrees.Tree_Type)
   renames Device_BTree_W.Check;
   procedure Stats
     (Tree                   : in out Device_BTrees.Tree_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer)
   renames Device_BTree_W.Stats;

   package Direct_BTree_W   is new Gen_Wrappers(Direct_IO);
   package Direct_BTrees    renames Direct_BTree_W.BTrees;
   procedure Check (T : in out Direct_BTrees.Tree_Type)
   renames Direct_BTree_W.Check;
   procedure Stats
     (Tree                   : in out Direct_BTrees.Tree_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer)
   renames Direct_BTree_W.Stats;

   package File_BTree_W     is new Gen_Wrappers(File_IO);
   package File_BTrees      renames File_BTree_W.BTrees;
   procedure Check (T : in out File_BTrees.Tree_Type)
   renames File_BTree_W.Check;
   procedure Stats
     (Tree                   : in out File_BTrees.Tree_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer)
   renames File_BTree_W.Stats;

   package File_SL_BTree_W  is new Gen_Wrappers(File_SL_IO);
   package File_SL_BTrees   renames File_SL_BTree_W.BTrees;
   procedure Check (T : in out File_SL_BTrees.Tree_Type)
   renames File_SL_BTree_W.Check;
   procedure Stats
     (Tree                   : in out File_SL_BTrees.Tree_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer)
   renames File_SL_BTree_W.Stats;

   package Memory_BTree_W   is new Gen_Wrappers(Memory_IO);
   package Memory_BTrees    renames Memory_BTree_W.BTrees;
   procedure Check (T : in out Memory_BTrees.Tree_Type)
   renames Memory_BTree_W.Check;
   procedure Stats
     (Tree                   : in out Memory_BTrees.Tree_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer)
   renames Memory_BTree_W.Stats;

   package Cmp_Mem_BTree_W  is new Gen_Wrappers(Cmp_Mem_IO);
   package Cmp_Mem_BTrees   renames Cmp_Mem_BTree_W.BTrees;
   procedure Check (T : in out Cmp_Mem_BTrees.Tree_Type)
   renames Cmp_Mem_BTree_W.Check;
   procedure Stats
     (Tree                   : in out Cmp_Mem_BTrees.Tree_Type;
      Height                 :    out Natural;
      Blocks                 :    out Natural;
      Free_Blocks            :    out Natural;
      Max_Degree             :    out Natural;
      Avg_Degree             :    out Natural;
      Min_Degree             :    out Natural;
      Bytes_Wasted_In_Blocks :    out Long_Integer;
      Bytes_In_Blocks        :    out Long_Integer)
   renames Cmp_Mem_BTree_W.Stats;

end DB.BTrees;

