with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with IO_Dispatcher.Args;
with IO_Dispatcher.Gen_BTrees;
with IO_Dispatcher.Gen_Blob_Trees;
with IO_Dispatcher.MMap;

with DB.IO.Blocks.Asynchronous_IO;
with DB.IO.Blocks.CFS_IO;
with DB.IO.Blocks.Compressed_Memory_IO;
with DB.IO.Blocks.Device_IO;
with DB.IO.Blocks.Direct_IO;
with DB.IO.Blocks.File_IO;
with DB.IO.Blocks.Memory_IO;

with DB.Utils.Traceback;

package body IO_Dispatcher is

   procedure Dispatch is

      ----------
      -- Helper types for procedure map.

      Max_Length : constant := 32;
      type String_Type is
         record
            Length : Natural;
            S      : String(1 .. Max_Length);
         end record;

      function New_String (S : String) return String_Type
      is
         Str : String_Type;
      begin
         Str.Length         := S'Length;
         Str.S(1..S'Length) := S;
         return Str;
      end New_String;

      function To_String (S : String_Type) return String is
      begin
         return S.S(1 .. S.Length);
      end To_String;

      function "=" (Left : String_Type; Right : String) return Boolean is
      begin
         return Left.Length = Right'Length and then
                Left.S(1..Left.Length) = Right;
      end "=";

      type TTree_Access is not null access procedure;

      type Entry_Type is
         record
            Name : String_Type := New_String("");
            Proc : TTree_Access;
         end record;

      type Entries_Type is array (Positive range <>) of Entry_Type;

      ----------
      -- Available procedures and their map.

      use DB.IO.Blocks;

      procedure Async_BTree    is new Gen_BTrees(Asynchronous_IO.IO);
      procedure CFS_BTree      is new Gen_BTrees(CFS_IO.IO);
      procedure Comp_Mem_BTree is new Gen_BTrees(Compressed_Memory_IO.IO);
      procedure Device_BTree   is new Gen_BTrees(Device_IO.IO);
      procedure Direct_BTree   is new Gen_BTrees(Direct_IO.IO);
      procedure File_BTree     is new Gen_BTrees(File_IO.IO);
      procedure File_Sys_BTree is new Gen_BTrees(File_IO.System_Locking_IO);
      procedure Memory_BTree   is new Gen_BTrees(Memory_IO.IO);

      procedure Async_Blob    is new Gen_Blob_Trees(Asynchronous_IO.IO);
      procedure CFS_Blob      is new Gen_Blob_Trees(CFS_IO.IO);
      procedure Comp_Mem_Blob is new Gen_Blob_Trees(Compressed_Memory_IO.IO);
      procedure Device_Blob   is new Gen_Blob_Trees(Device_IO.IO);
      procedure Direct_Blob   is new Gen_Blob_Trees(Direct_IO.IO);
      procedure File_Blob     is new Gen_Blob_Trees(File_IO.IO);
      procedure File_Sys_Blob is new Gen_Blob_Trees(File_IO.System_Locking_IO);
      procedure Memory_Blob   is new Gen_Blob_Trees(Memory_IO.IO);

      Procs : constant Entries_Type
            := ((New_String("async"),        Async_BTree'Access),
                (New_String("cfs"),          CFS_BTree'Access),
                (New_String("compmem"),      Comp_Mem_BTree'Access),
                (New_String("device"),       Device_BTree'Access),
                (New_String("direct"),       Direct_BTree'Access),
                (New_String("file"),         File_BTree'Access),
                (New_String("filesys"),      File_Sys_BTree'Access),
                (New_String("memory"),       Memory_BTree'Access),

                (New_String("blob_async"),   Async_Blob'Access),
                (New_String("blob_cfs"),     CFS_Blob'Access),
                (New_String("blob_compmem"), Comp_Mem_Blob'Access),
                (New_String("blob_device"),  Device_Blob'Access),
                (New_String("blob_direct"),  Direct_Blob'Access),
                (New_String("blob_file"),    File_Blob'Access),
                (New_String("blobsys"),      File_Sys_Blob'Access),
                (New_String("blob_memory"),  Memory_Blob'Access),

                (New_string("mmap"),    MMap'Access));

      IO_Name : constant String := Args.Pop_Argument(1);
   begin
      for I in Procs'Range loop
         if Procs(I).Name = IO_Name then
            Put_Line(To_String(Procs(I).Name));
            Procs(I).Proc.all;
            return;
         end if;
      end loop;
      Args.Undo_Pop;
      Put_Line("No valid IO for "& IO_Name &" found");
   exception
      when Error : others =>
         Put_Line("Exception: "& Exception_Message(Error));
         Put_Line("Exception: "& Exception_Information(Error));
         DB.Utils.Traceback.Print_Traceback(Error);
   end Dispatch;

end IO_Dispatcher;

