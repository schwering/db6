package body DB.Utils.Bounded_Pools is

   overriding procedure Allocate
     (Pool         : in out Bounded_No_Reclaim_Pool;
      Address      :    out System.Address;
      Storage_Size : in     SSE.Storage_Count;
      Alignment    : in     SSE.Storage_Count)
   is
      use type SSE.Storage_Count;
   begin
      if Pool.Current_Storage_Size + Storage_Size > Pool.Max_Storage_Size then
         raise Storage_Error;
      end if;
      SPG.Unbounded_No_Reclaim_Pool(Pool).Allocate(Address, Storage_Size,
                                                   Alignment);
      Pool.Current_Storage_Size := Pool.Current_Storage_Size + Storage_Size;
   end Allocate;


   overriding procedure Deallocate
     (Pool         : in out Bounded_No_Reclaim_Pool;
      Address      : in     System.Address;
      Storage_Size : in     SSE.Storage_Count;
      Alignment    : in     SSE.Storage_Count)
   is
      use type SSE.Storage_Count;
   begin
      SPG.Unbounded_No_Reclaim_Pool(Pool).Deallocate(Address, Storage_Size,
                                                     Alignment);
      Pool.Current_Storage_Size := Pool.Current_Storage_Size - Storage_Size;
   end Deallocate;


   overriding function Storage_Size
     (Pool : Bounded_No_Reclaim_Pool)
      return SSE.Storage_Count
   is begin
      return Pool.Max_Storage_Size;
   end Storage_Size;

end DB.Utils.Bounded_Pools;

