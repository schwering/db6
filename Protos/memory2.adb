with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with System.Pool_Global;
with System.Storage_Elements;

procedure Memory2
is
   package SSE renames System.Storage_Elements;


   package Bounded_Pool is
      package SSE renames System.Storage_Elements;
      package SPG renames System.Pool_Global;

      type Bounded_No_Reclaim_Pool (Max_Storage_Size : SSE.Storage_Count) is
      new SPG.Unbounded_No_Reclaim_Pool with private;

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

      private
         type Bounded_No_Reclaim_Pool (Max_Storage_Size : SSE.Storage_Count) is
         new SPG.Unbounded_No_Reclaim_Pool with
         record
            Current_Storage_Size : SSE.Storage_Count := 0;
         end record;
   end Bounded_Pool;


   package body Bounded_Pool is
      overriding procedure Allocate
        (Pool         : in out Bounded_No_Reclaim_Pool;
         Address      :    out System.Address;
         Storage_Size : in     SSE.Storage_Count;
         Alignment    : in     SSE.Storage_Count)
      is
         use type SSE.Storage_Count;
         use type System.Address;
      begin
         --Put_Line("Allocate"& Storage_Size'Img & Alignment'Img);
         if Pool.Current_Storage_Size + Storage_Size > Pool.Max_Storage_Size
         then
            raise Storage_Error;
         end if;
         SPG.Unbounded_No_Reclaim_Pool(Pool).Allocate
            (Address, Storage_Size, Alignment);
         Pool.Current_Storage_Size :=
            Pool.Current_Storage_Size + Storage_Size;
         if Address = System.Null_Address then
            raise Storage_Error;
         end if;
      end Allocate;

      overriding procedure Deallocate
        (Pool         : in out Bounded_No_Reclaim_Pool;
         Address      : in     System.Address;
         Storage_Size : in     SSE.Storage_Count;
         Alignment    : in     SSE.Storage_Count)
      is
         use type SSE.Storage_Count;
      begin
         --Put_Line("Deallocate"& Storage_Size'Img & Alignment'Img);
         SPG.Unbounded_No_Reclaim_Pool(Pool).Deallocate
            (Address, Storage_Size, Alignment);
         Pool.Current_Storage_Size :=
            Pool.Current_Storage_Size - Storage_Size;
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

   subtype Block_Type is SSE.Storage_Array;
   type Block_Ref_Type is access Block_Type;
   for Block_Ref_Type'Storage_Pool use Pool;

   Size : SSE.Storage_Count := 0;
begin
   for I in Positive loop
      declare
         S : constant SSE.Storage_Count := Pool.Max_Storage_Size / 1024;
         B : Block_Ref_Type;
      begin
         B := new Block_Type(1 .. S);
         Size := Size + S;
      exception
         when Error : others =>
            Put_Line("Size bytes:      "&
                     Size'Img);
            Put_Line("Max bytes:       "&
                     SSE.Storage_Count'Image(Pool.Max_Storage_Size));
            Put_Line("Size kilobytes:  "&
                     SSE.Storage_Count'Image(Size/1024));
            Put_Line("Max kilobytes:   "&
                     SSE.Storage_Count'Image(Pool.Max_Storage_Size/1024));
            Put_Line("Size megabytes:  "&
                     SSE.Storage_Count'Image(Size/1024/1024));
            Put_Line("Max megabytes:   "&
                     SSE.Storage_Count'Image(Pool.Max_Storage_Size/1024/1024));
            Put_Line("Size gigabytes:  "&
                     SSE.Storage_Count'Image(Size/1024/1024/1024));
            Put_Line("Max gigabytes:   "&
                     SSE.Storage_Count'Image(Pool.Max_Storage_Size/1024/1024/1024));
            Put_Line("Exception: "& Exception_Message(Error));
            Put_Line("Exception: "& Exception_Information(Error));
            exit;
      end;
   end loop;
end Memory2;

