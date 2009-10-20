with System.Pool_Global;
with System.Storage_Elements;

package DB.Util.Bounded_Pool is
   pragma Elaborate_Body;

   package SSE renames System.Storage_Elements;
   package SPG renames System.Pool_Global;

   type Bounded_No_Reclaim_Pool (Max_Storage_Size : SSE.Storage_Count) is
      new SPG.Unbounded_No_Reclaim_Pool with
      record
         Current_Storage_Size : SSE.Storage_Count := 0;
      end record;

   overriding procedure Allocate
     (Pool         : in out Bounded_No_Reclaim_Pool;
      Address      :    out System.Address;
      Storage_Size : in     SSE.Storage_Count;
      Alignment    : in     SSE.Storage_Count);

   overriding procedure Deallocate
     (Pool         : in out Bounded_No_Reclaim_Pool;
      Address      : in     System.Address;
      Storage_Size : in     SSE.Storage_Count;
      Alignment    : in     SSE.Storage_Count);

   overriding function Storage_Size
     (Pool : Bounded_No_Reclaim_Pool)
      return SSE.Storage_Count;

end DB.Util.Bounded_Pool;

