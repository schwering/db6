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
         Put_Line("Allocate"& Storage_Size'Img & Alignment'Img);
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
         Put_Line("Deallocate"& Storage_Size'Img & Alignment'Img);
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

   --subtype Block_Type is SSE.Storage_Array(1 .. 37748868);
   type Key_Type is mod 2**64;
   type Value_Type is access SSE.Storage_Array;
   subtype Size_Type is Natural;
   type Hash_Type is mod 2**32;
   type State_Type is (Used, Visited, Free);
   type Element_Type (State : State_Type := Used) is
      record
         case State is
            when Used =>
               Key   : Key_Type;
               Value : Value_Type;
            when others =>
               null;
         end case;
      end record;
   type Array_Type is array (Hash_Type range <>) of Element_Type;
   type Block_Type (Capacity : Hash_Type) is limited
      record
         Arr    : Array_Type(0 .. Capacity)
                := (others => Element_Type'(State => Free));
         Size   : Size_Type := 0;
         Visits : Size_Type := 0;
      end record;
   type Block_Access_Type is access Block_Type;
   for Block_Access_Type'Storage_Pool use Pool;

   function To_Prime
     (Size : Size_Type)
      return Hash_Type
   is
      Primes : constant array (Positive range <>) of Hash_Type :=
        (53,         97,         193,       389,       769,
         1543,       3079,       6151,      12289,     24593,
         49157,      98317,      196613,    393241,    786433,
         1572869,    3145739,    6291469,   12582917,  25165843,
         50331653,   100663319,  201326611, 402653189, 805306457,
         1610612741, 3221225473, 4294967291);
   begin
      for I in Primes'Range loop
         if Primes(I) >= Hash_Type(Size) then
            return Primes(I);
         end if;
      end loop;
      raise Constraint_Error;
   end To_Prime;

   function New_Block
     (Size : Size_Type)
      return Block_Type
   is
   begin
      return Block_Type'(Capacity => Hash_Type(Size) - 1, others => <>);
   end New_Block;

begin
   Put_Line("Component_Size ="& Array_Type'Component_Size'Img);
   for I in Positive loop
      declare
         B : Block_Access_Type;
      begin
         Put_Line("I:"& I'Img);
         B := new Block_Type'(To_Prime(3145738)-1);
         Put_Line("B.Arr'Length ="& B.Arr'Length'Img &" "&
                  Boolean'Image(B.Arr'Length = To_Prime(3145738)));
         if I mod 2 = 0 then
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

