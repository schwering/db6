-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Interfaces.C;
with Interfaces.C_Streams;
with System;

package body DB.Blocks.Low_Level_IO is

   package C is
      subtype int is Interfaces.C.int;
      subtype char_ptr is Interfaces.C.char_array;
      subtype void_ptr is Interfaces.C_Streams.voids;
      subtype size_t is Interfaces.C.size_t;
      type ssize_t is
         range -(2 ** (size_t'Size-1)) .. +(2 ** (size_t'Size-1)) - 1;
      for ssize_t'Size use size_t'Size;
      type off_t is mod 2 ** 64;
      --type off_t is mod 2 ** 32;

      FLAGS_CREATE : constant int;
      pragma Import (C, FLAGS_CREATE, "db_blocks_low_level_io_create_flags");

      FLAGS_OPEN_RO : constant int;
      pragma Import (C, FLAGS_OPEN_RO, "db_blocks_low_level_io_open_ro_flags");
      FLAGS_OPEN_RW : constant int;
      pragma Import (C, FLAGS_OPEN_RW, "db_blocks_low_level_io_open_rw_flags");

      MODE : constant int;
      pragma Import (C, MODE, "db_blocks_low_level_io_file_mode");

      function open (Path : char_ptr; Flags : int; Mode : int) return int;
      pragma Import (C, open, "db_blocks_low_level_io_open");

      function unlink (Path : char_ptr) return int;
      pragma Import (C, unlink, "db_blocks_low_level_io_unlink");

      function close (FD : int) return int;
      pragma Import (C, close, "db_blocks_low_level_io_close");

      function pread (FD : int; Buf : void_ptr; NBytes : size_t;
                      Offset : off_t) return ssize_t;
      pragma Import(C, pread, "db_blocks_low_level_io_pread");

      function pwrite (FD : int; Buf : void_ptr; NBytes : size_t;
                       Offset : off_t) return ssize_t;
      pragma Import (C, pwrite, "db_blocks_low_level_io_pwrite");

      function alloc (FD : int; NBytes : size_t) return off_t;
      pragma Import (C, alloc, "db_blocks_low_level_io_alloc");

      function lock (FD : int; Offset : off_t; NBytes : size_t) return int;
      pragma Import (C, lock, "db_blocks_low_level_io_lock");

      function unlock (FD : int; Offset : off_t; NBytes : size_t) return int;
      pragma Import (C, unlock, "db_blocks_low_level_io_unlock");

      function errno return int;
      pragma Import (C, errno, "db_blocks_low_level_io_errno");

      pragma Warnings (Off);
      function strerror (Errno : int) return char_ptr;
      pragma Import (C, strerror, "strerror");
      pragma Warnings (On);

      function To_Ada (S : char_ptr; Trim_Nul : Boolean := True) return String
         renames Interfaces.C.To_Ada;

      function To_C (S : String; Append_Nul : Boolean := True) return char_ptr
         renames Interfaces.C.To_C;
   end C;


   procedure Open
     (Path      : in  String;
      Open_Kind : in  Open_Kind_Type := Read_Write;
      File      : out File_Descriptor_Type)
   is
      use type C.int;
      Str   : constant C.char_ptr := C.To_C(Path);
      Flags : C.int;
      FD    : C.int;
   begin
      case Open_Kind is
         when Create =>     Flags := C.FLAGS_CREATE;
         when Read_Only =>  Flags := C.FLAGS_OPEN_RO;
         when Read_Write => Flags := C.FLAGS_OPEN_RW;
      end case;
      FD := C.open(Str, Flags, C.MODE);
      if FD = -1 then
         raise IO_Error;
      end if;
      File := File_Descriptor_Type(FD);
   end Open;


   procedure Unlink
     (Path : in String)
   is
      use type C.int;
   begin
      if C.unlink(C.To_C(Path)) /= 0 then
         raise IO_Error;
      end if;
   end Unlink;


   procedure Close
     (File : in File_Descriptor_Type)
   is
      use type C.int;
   begin
      if C.close(C.int(File)) /= 0 then
         raise IO_Error;
      end if;
   end Close;


   procedure PRead
     (File : in  File_Descriptor_Type;
      Pos  : in  File_Position_Type;
      Item : out Item_Type)
   is
      Size  : constant Size_Type := Item'Size / System.Storage_Unit;
      Count : constant Signed_Size_Type :=
         Signed_Size_Type(C.pread(C.int(File),
                                  Item'Address,
                                  C.size_t(Size),
                                  C.off_t(Pos)));
   begin
      if Count < 0 or else Size_Type(Count) /= Size then
         raise IO_Error;
      end if;
   end PRead;


   procedure PWrite
     (File : in File_Descriptor_Type;
      Pos  : in File_Position_Type;
      Item : in Item_Type)
   is
      Size  : constant Size_Type := Item'Size / System.Storage_Unit;
      Count : constant Signed_Size_Type :=
         Signed_Size_Type(C.Pwrite(C.int(File),
                                   Item'Address,
                                   C.size_t(Size),
                                   C.off_t(Pos)));
   begin
      if Count < 0 or else Size_Type(Count) /= Size then
         raise IO_Error;
      end if;
   end PWrite;


   procedure Allocate
     (File   : in  File_Descriptor_Type;
      Length : in  Size_Type;
      Pos    : out File_Position_Type) is
   begin
      Pos := File_Position_Type(C.alloc(C.int(File), C.size_t(Length)));
      if Pos = File_Position_Type'Last then
         raise IO_Error;
      end if;
   end Allocate;


   procedure Lock
     (File   : in File_Descriptor_Type;
      Pos    : in File_Position_Type;
      Length : in Size_Type)
   is
      use type C.int;
   begin
      if C.lock(C.int(File), C.off_t(Pos), C.size_t(Length)) = -1 then
         raise IO_Error;
      end if;
   end Lock;


   procedure Unlock
     (File   : in File_Descriptor_Type;
      Pos    : in File_Position_Type;
      Length : in Size_Type)
   is
      use type C.int;
   begin
      if C.unlock(C.int(File), C.off_t(Pos), C.size_t(Length)) = -1 then
         raise IO_Error;
      end if;
   end Unlock;


   function Strerror
      return String
   is
      use type C.int;
   begin
      if C.errno /= 0 then
         return C.To_Ada(C.strerror(C.errno));
      else
         return "(No error)";
      end if;
   end Strerror;

end DB.Blocks.Low_Level_IO;

