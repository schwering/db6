-- Abstract:
--
-- Storage pool that just forwards to DB.Utils.Global_Pool.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System;
with System.Storage_Pools;
with System.Storage_Elements;

package DB.Utils.Debug_Storage_Pools is
   pragma Preelaborate;

   package SSP renames System.Storage_Pools;
   package SSE renames System.Storage_Elements;

   type Debug_Storage_Pool_Type is new SSP.Root_Storage_Pool with private;

   overriding
   procedure Allocate
     (Pool                     : in out Debug_Storage_Pool_Type;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements : in     SSE.Storage_Count;
      Alignment                : in     SSE.Storage_Count);

   overriding
   procedure Deallocate
     (Pool                     : in out Debug_Storage_Pool_Type;
      Storage_Address          : in     System.Address;
      Size_In_Storage_Elements : in     SSE.Storage_Count;
      Alignment                : in     SSE.Storage_Count);

   overriding
   function Storage_Size
     (Pool : Debug_Storage_Pool_Type)
      return SSE.Storage_Count;

   function Count (Pool : Debug_Storage_Pool_Type) return Integer;

private
   type Debug_Storage_Pool_Type is new SSP.Root_Storage_Pool with
      record
         Count : Integer := 0;
      end record;

end DB.Utils.Debug_Storage_Pools;

