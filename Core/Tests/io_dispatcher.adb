with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with IO_Dispatcher.Args;
with IO_Dispatcher.Gen_BTrees;
with IO_Dispatcher.Gen_Blob_Trees;
with IO_Dispatcher.Map;
with IO_Dispatcher.Map_Cursor;

with DB.BTrees;
with DB.Blob_Trees;

with DB.Utils.Traceback;

package body IO_Dispatcher
is
   procedure Dispatch
   is
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

      use DB.BTrees;
      use DB.Blob_Trees;

      procedure Async_BTree    is new Gen_BTrees(Async_BTrees, Check, Stats);
      procedure CFS_BTree      is new Gen_BTrees(CFS_BTrees, Check, Stats);
      procedure Cmp_Mem_BTree  is new Gen_BTrees(Cmp_Mem_BTrees, Check, Stats);
      procedure Device_BTree   is new Gen_BTrees(Device_BTrees, Check, Stats);
      procedure Direct_BTree   is new Gen_BTrees(Direct_BTrees, Check, Stats);
      procedure File_BTree     is new Gen_BTrees(File_BTrees, Check, Stats);
      procedure File_SL_BTree  is new Gen_BTrees(File_SL_BTrees, Check, Stats);
      procedure Memory_BTree   is new Gen_BTrees(Memory_BTrees, Check, Stats);

      procedure Async_Blob    is new Gen_Blob_Trees(Async_Blob_Trees, Check);
      procedure CFS_Blob      is new Gen_Blob_Trees(CFS_Blob_Trees, Check);
      procedure Cmp_Mem_Blob  is new Gen_Blob_Trees(Cmp_Mem_Blob_Trees, Check);
      procedure Device_Blob   is new Gen_Blob_Trees(Device_Blob_Trees, Check);
      procedure Direct_Blob   is new Gen_Blob_Trees(Direct_Blob_Trees, Check);
      procedure File_Blob     is new Gen_Blob_Trees(File_Blob_Trees, Check);
      procedure File_SL_Blob  is new Gen_Blob_Trees(File_SL_Blob_Trees, Check);
      procedure Memory_Blob   is new Gen_Blob_Trees(Memory_Blob_Trees, Check);

      Procs : constant Entries_Type
            := ((New_String("async"),        Async_BTree'Access),
                (New_String("cfs"),          CFS_BTree'Access),
                (New_String("compmem"),      Cmp_Mem_BTree'Access),
                (New_String("device"),       Device_BTree'Access),
                (New_String("direct"),       Direct_BTree'Access),
                (New_String("file"),         File_BTree'Access),
                (New_String("filesl"),       File_SL_BTree'Access),
                (New_String("memory"),       Memory_BTree'Access),

                (New_String("blob_async"),   Async_Blob'Access),
                (New_String("blob_cfs"),     CFS_Blob'Access),
                (New_String("blob_compmem"), Cmp_Mem_Blob'Access),
                (New_String("blob_device"),  Device_Blob'Access),
                (New_String("blob_direct"),  Direct_Blob'Access),
                (New_String("blob_file"),    File_Blob'Access),
                (New_String("blob_filesl"),  File_SL_Blob'Access),
                (New_String("blob_memory"),  Memory_Blob'Access),

                (New_String("map"),          Map'Access),
                (New_String("map_cursor"),   Map_Cursor'Access));

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

