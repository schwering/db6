with System.Storage_Elements;

package DB.Compression.Deflate is
   pragma Preelaborate;

   subtype Buffer_Type is System.Storage_Elements.Storage_Array;
   subtype Size_Type is System.Storage_Elements.Storage_Offset;

   function Worst_Deflate_Overhead 
     (Uncompressed_Size : Size_Type)
      return Size_Type;

   function Deflate
     (Buffer                  : Buffer_Type;
      Max_Uncompressed_Length : Size_Type)
      return Buffer_Type;

   function Inflate
     (Buffer                : Buffer_Type;
      Max_Compressed_Length : Size_Type)
      return Buffer_Type;

private
   subtype Index_Type is System.Storage_Elements.Storage_Offset;
   use type System.Storage_Elements.Storage_Offset;

end DB.Compression.Deflate;

