package body Gen_Circular_Buffers is

   function Top (C : Circular_Buffer_Type) return Item_Type
   is begin
   end Top;


   procedure Pop (C : in out Circular_Buffer_Type)
   is begin
   end Pop;


   procedure Pop (C : in out Circular_Buffer_Type; Item : out Item_Type)
   is begin
   end Pop;


   procedure Push (C : in out Circular_Buffer_Type; Item : in Item_Type)
   is begin
   end Push;


   procedure Add (C : in out Circular_Buffer_Type; Item : in Item_Type)
   renames Push;


   procedure Remove (C : in out Circular_Buffer_Type; Item : in Item_Type)
   is begin
   end Remove;


   function Contains (C : Circular_Buffer_Type; Item : Item_Type)
      return Boolean
   is begin
      for I in C.Pop_Index .. C.Pop_Index
   end Remove;


   function Is_Empty (C : Circular_Buffer_Type) return Boolean
   is begin
      return C.Pop_Index = C.Push_Index and not C.Full;
   end Is_Empty;


   function Is_Full (C : Circular_Buffer_Type) return Boolean
   is begin
      return C.Pop_Index = C.Push_Index and not C.Full;
   end Is_Full;


   function Size (C : Circular_Buffer_Type) return Size_Type
   is begin
      if C.Pop_Index <= C.Push_Index then
         return C.Push_Index - C.Pop_Index;
      else
         return C.Capacity - C.Pop_Index + 1 + C.Push_Index - 1;
   end Remove;

end Gen_Circular_Buffers;

