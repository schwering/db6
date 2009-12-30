-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with Interfaces.C;
with Interfaces.C_Streams;

package body DB.Compression.Deflate is

   package C is
      subtype int is Interfaces.C.int;
      subtype void_ptr is Interfaces.C_Streams.voids;

      function inflate (Src : void_ptr; Srclen : int;
                        Dst : void_ptr; Dstlen : int) return int;
      pragma Import (C, inflate, "db_compression_inflate");

      function deflate (Src : void_ptr; Srclen : int;
                        Dst : void_ptr; Dstlen : int) return int;
      pragma Import (C, deflate, "db_compression_deflate");
   end C;


   function Worst_Deflate_Overhead
     (Uncompressed_Size : Size_Type)
      return Size_Type
   is
      Deflate_Block_Size : constant := 16 * 2**10;
   begin
      return 6 + 5 * (Uncompressed_Size + Deflate_Block_Size - 1) /
                     Deflate_Block_Size;
   end Worst_Deflate_Overhead;


   function Deflate
     (Buffer                  : Buffer_Type;
      Max_Uncompressed_Length : Size_Type)
      return Buffer_Type
   is
      Dest     : Buffer_Type(1 .. Max_Uncompressed_Length);
      Dest_Len : constant C.int := C.deflate(Buffer'Address, Buffer'Length,
                                             Dest'Address, Dest'Length);
   begin
      return Dest(1 .. Index_Type(Dest_Len));
   end Deflate;


   function Inflate
     (Buffer                : Buffer_Type;
      Max_Compressed_Length : Size_Type)
      return Buffer_Type
   is
      Dest     : Buffer_Type(1 .. Max_Compressed_Length);
      Dest_Len : constant C.int := C.inflate(Buffer'Address, Buffer'Length,
                                             Dest'Address, Dest'Length);
   begin
      return Dest(1 .. Index_Type(Dest_Len));
   end;

end DB.Compression.Deflate;

