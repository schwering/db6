-- Abstract:
--
-- see spec
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils.Global_Pool;

package body DB.Utils.Debug_Storage_Pools is

   package GP renames Global_Pool;

   overriding
   procedure Allocate
     (Pool                     : in out Debug_Storage_Pool_Type;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements : in     SSE.Storage_Count;
      Alignment                : in     SSE.Storage_Count) is
   begin
      GP.Global_Storage_Pool.Allocate
        (Storage_Address, Size_In_Storage_Elements, Alignment);
      Pool.Count := Pool.Count + 1;
   end Allocate;


   overriding
   procedure Deallocate
     (Pool                     : in out Debug_Storage_Pool_Type;
      Storage_Address          : in     System.Address;
      Size_In_Storage_Elements : in     SSE.Storage_Count;
      Alignment                : in     SSE.Storage_Count) is
   begin
      GP.Global_Storage_Pool.Deallocate
        (Storage_Address, Size_In_Storage_Elements, Alignment);
      Pool.Count := Pool.Count - 1;
   end Deallocate;


   overriding
   function Storage_Size
     (Pool : Debug_Storage_Pool_Type)
      return SSE.Storage_Count is
   begin
      return GP.Global_Storage_Pool.Storage_Size;
   end Storage_Size;


   function Count (Pool : Debug_Storage_Pool_Type) return Integer is
   begin
      return Pool.Count;
   end Count;

end DB.Utils.Debug_Storage_Pools;

