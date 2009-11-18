-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009 Christoph Schwering

with Interfaces.C;
with Interfaces.C_Streams;
with System;

package body DB.IO.Low_Level is

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
      pragma Import (C, FLAGS_CREATE, "db_io_low_level_create_flags");

      FLAGS_OPEN_RO : constant int;
      pragma Import (C, FLAGS_OPEN_RO, "db_io_low_level_open_ro_flags");

      FLAGS_OPEN_RW : constant int;
      pragma Import (C, FLAGS_OPEN_RW, "db_io_low_level_open_rw_flags");

      FLAGS_CREATE_DIRECT : constant int;
      pragma Import (C, FLAGS_CREATE_DIRECT,
                     "db_io_low_level_create_direct_flags");

      FLAGS_OPEN_RO_DIRECT : constant int;
      pragma Import (C, FLAGS_OPEN_RO_DIRECT,
                     "db_io_low_level_open_ro_direct_flags");

      FLAGS_OPEN_RW_DIRECT : constant int;
      pragma Import (C, FLAGS_OPEN_RW_DIRECT,
                     "db_io_low_level_open_rw_direct_flags");

      MODE : constant int;
      pragma Import (C, MODE, "db_io_low_level_file_mode");

      SEEK_SET : constant int;
      pragma Import (C, SEEK_SET, "db_io_low_level_seek_set");

      SEEK_CUR : constant int;
      pragma Import (C, SEEK_CUR, "db_io_low_level_seek_cur");

      SEEK_END : constant int;
      pragma Import (C, SEEK_END, "db_io_low_level_seek_end");

      function open (Path : char_ptr; Flags : int; Mode : int) return int;
      pragma Import (C, open, "db_io_low_level_open");

      function close (FD : int) return int;
      pragma Import (C, close, "db_io_low_level_close");

      function lock (FD : int; Excl : int; Wait : int) return int;
      pragma Import (C, lock, "db_io_low_level_lock");

      function unlock (FD : int) return int;
      pragma Import (C, unlock, "db_io_low_level_unlock");

      function size (FD : int) return off_t;
      pragma Import (C, size, "db_io_low_level_size");

      function lseek (FD : int; Pos : off_t; Kind : int) return off_t;
      pragma Import (C, lseek, "db_io_low_level_lseek");

      function read (FD : int; Buf : void_ptr; NBytes : size_t) return ssize_t;
      pragma Import(C, read, "db_io_low_level_read");

      function pread (FD : int; Buf : void_ptr; NBytes : size_t;
                      Offset : off_t) return ssize_t;
      pragma Import(C, pread, "db_io_low_level_pread");

      function write (FD : int; Buf : void_ptr; NBytes : size_t) return ssize_t;
      pragma Import (C, write, "db_io_low_level_write");

      function pwrite (FD : int; Buf : void_ptr; NBytes : size_t;
                       Offset : off_t) return ssize_t;
      pragma Import (C, pwrite, "db_io_low_level_pwrite");

      function read_direct (FD : int; Buf : void_ptr;
                            NBytes : size_t) return ssize_t;
      pragma Import(C, read_direct, "db_io_low_level_read_direct");

      function write_direct (FD : int; Buf : void_ptr;
                             NBytes : size_t) return ssize_t;
      pragma Import (C, write_direct, "db_io_low_level_write_direct");

      function errno return int;
      pragma Import (C, errno, "db_io_low_level_errno");

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


   procedure Open_Direct
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
         when Create =>     Flags := C.FLAGS_CREATE_DIRECT;
         when Read_Only =>  Flags := C.FLAGS_OPEN_RO_DIRECT;
         when Read_Write => Flags := C.FLAGS_OPEN_RW_DIRECT;
      end case;
      FD := C.open(Str, Flags, C.MODE);
      if FD = -1 then
         raise IO_Error;
      end if;
      File := File_Descriptor_Type(FD);
   end Open_Direct;


   procedure Close
     (File : in File_Descriptor_Type)
   is
      use type C.int;
   begin
      if C.close(C.int(File)) /= 0 then
         raise IO_Error;
      end if;
   end Close;


   procedure Lock
     (File     : in  File_Descriptor_Type;
      Kind     : in  Lock_Kind_Type;
      Blocking : in  Boolean;
      Success  : out Boolean)
   is
      use type C.int;
      Excl : C.int;
      Wait : C.int;
   begin
      case Kind is
         when Exclusive => Excl := 1;
         when Shared =>    Excl := 0;
      end case;
      case Blocking is
         when True =>  Wait := 1;
         when False => Wait := 0;
      end case;
      Success := C.lock(C.int(File), Excl, Wait) = 0;
   end Lock;


   procedure Unlock
     (File    : in  File_Descriptor_Type;
      Success : out Boolean)
   is
      use type C.int;
   begin
      Success := C.unlock(C.int(File)) = 0;
   end Unlock;


   procedure Get_Size
     (File          : in  File_Descriptor_Type;
      Last_Position : out File_Position_Type)
   is
      use type C.off_t;
      Size : constant C.off_t := C.size(C.int(File));
   begin
      if Size = C.off_t'Last then
         raise IO_Error;
      end if;
      Last_Position := File_Position_Type(Size);
   end Get_Size;


   procedure Seek
     (File : in File_Descriptor_Type;
      Pos  : in File_Position_Type;
      Kind : in Seek_Kind_Type := From_Beginning)
   is
      New_Pos : File_Position_Type;
      pragma Unreferenced (New_Pos);
   begin
      Seek(File, Pos, Kind, New_Pos);
   end Seek;


   procedure Seek
     (File    : in  File_Descriptor_Type;
      Pos     : in  File_Position_Type;
      Kind    : in  Seek_Kind_Type := From_Beginning;
      New_Pos : out File_Position_Type)
   is
      Whence : C.int;
      Offset : C.off_t;
      use type C.off_t;
   begin
      case Kind is
         when From_Beginning => Whence := C.SEEK_SET;
         when From_Position  => Whence := C.SEEK_CUR;
         when From_End       => Whence := C.SEEK_END;
      end case;
      Offset := C.lseek(C.int(File), C.off_t(Pos), Whence);
      if Offset = C.off_t'Last then
         raise IO_Error;
      end if;
      New_Pos := File_Position_Type(Offset);
   end Seek;


   procedure Seek_End
     (File    : in  File_Descriptor_Type;
      New_Pos : out File_Position_Type) is
   begin
      Seek(File, 0, From_End, New_Pos);
   end Seek_End;


   function Current_File_Position
     (File : File_Descriptor_Type)
      return File_Position_Type is
   begin
      return File_Position_Type(C.lseek(C.int(File), C.off_t(0), C.SEEK_CUR));
   end Current_File_Position;


   procedure Read
     (File : in  File_Descriptor_Type;
      Item : out Item_Type)
   is
      Size  : constant Size_Type := Item'Size / System.Storage_Unit;
      Count : constant Signed_Size_Type
            := Signed_Size_Type(C.read(C.int(File),
                                       Item'Address,
                                       C.size_t(Size)));
   begin
      if Count < 0 or else Size_Type(Count) /= Size then
         raise IO_Error;
      end if;
   end Read;


   procedure PRead
     (File : in  File_Descriptor_Type;
      Pos  : in  File_Position_Type;
      Item : out Item_Type)
   is
      Size  : constant Size_Type := Item'Size / System.Storage_Unit;
      Count : constant Signed_Size_Type
            := Signed_Size_Type(C.pread(C.int(File),
                                        Item'Address,
                                        C.size_t(Size),
                                        C.off_t(Pos)));
   begin
      if Count < 0 or else Size_Type(Count) /= Size then
         raise IO_Error;
      end if;
   end PRead;


   procedure Write
     (File : in File_Descriptor_Type;
      Item : in Item_Type)
   is
      Size  : constant Size_Type := Item'Size / System.Storage_Unit;
      Count : constant Signed_Size_Type
            := Signed_Size_Type(C.write(C.int(File),
                                        Item'Address,
                                        C.size_t(Size)));
   begin
      if Count < 0 or else Size_Type(Count) /= Size then
         raise IO_Error;
      end if;
   end Write;


   procedure PWrite
     (File : in File_Descriptor_Type;
      Pos  : in File_Position_Type;
      Item : in Item_Type)
   is
      Size  : constant Size_Type := Item'Size / System.Storage_Unit;
      Count : constant Signed_Size_Type
            := Signed_Size_Type(C.Pwrite(C.int(File),
                                         Item'Address,
                                         C.size_t(Size),
                                         C.off_t(Pos)));
   begin
      if Count < 0 or else Size_Type(Count) /= Size then
         raise IO_Error;
      end if;
   end PWrite;


   procedure Read_Direct
     (File : in  File_Descriptor_Type;
      Item : out Item_Type)
   is
      Size  : constant Size_Type := Item'Size / System.Storage_Unit;
      Count : constant Signed_Size_Type
            := Signed_Size_Type(C.read_direct(C.int(File),
                                              Item'Address,
                                              C.size_t(Size)));
   begin
      if Count < 0 or else Size_Type(Count) /= Size then
         raise IO_Error;
      end if;
   end Read_Direct;


   procedure Write_Direct
     (File : in File_Descriptor_Type;
      Item : in Item_Type)
   is
      Size  : constant Size_Type := Item'Size / System.Storage_Unit;
      Count : constant Signed_Size_Type
            := Signed_Size_Type(C.write_direct(C.int(File),
                                               Item'Address,
                                               C.size_t(Size)));
   begin
      if Count < 0 or else Size_Type(Count) /= Size then
         raise IO_Error;
      end if;
   end Write_Direct;


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

end DB.IO.Low_Level;

