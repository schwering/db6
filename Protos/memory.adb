with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with System.Pool_Global;
with System.Storage_Elements;

with System.Storage_Elements;

procedure Memory
is
   package SSE renames System.Storage_Elements;


   package Bounded_Pool is
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

   end Bounded_Pool;



   package body Bounded_Pool is

      overriding procedure Allocate
        (Pool         : in out Bounded_No_Reclaim_Pool;
         Address      :    out System.Address;
         Storage_Size : in     SSE.Storage_Count;
         Alignment    : in     SSE.Storage_Count)
      is
         use type SSE.Storage_Count;
      begin
         if Pool.Current_Storage_Size+Storage_Size > Pool.Max_Storage_Size then
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

   end Bounded_Pool;


   Kilo : constant := 2**10;
   Mega : constant := 2**10 * Kilo;
   Giga : constant := 2**10 * Mega;

   use type SSE.Storage_Count;
   Pool : Bounded_Pool.Bounded_No_Reclaim_Pool(Giga * 2 - 2 * Mega);

   subtype Block_Type is SSE.Storage_Array(1 .. Mega);
   type Block_Access_Type is access Block_Type;
   for Block_Access_Type'Storage_Pool use Pool;
begin

   for I in Positive loop
      declare
         B : Block_Access_Type;
      begin
         B := new Block_Type;
         if False and I mod 2 = 0 then
            declare
               procedure Free is new Ada.Unchecked_Deallocation
                  (Block_Type, Block_Access_Type);
            begin
               Free(B);
            end;
         end if;
      exception
         when Error : others =>
            Put_Line("I:"& I'Img);
            Put_Line("Exception: "& Exception_Message(Error));
            Put_Line("Exception: "& Exception_Information(Error));
            exit;
      end;
   end loop;

end Memory;

