package body Gen_Containers.Gen_Vectors is

   overriding procedure Add (V : in out Vector_Type; Item : in Item_Type)
   is begin
      if Is_Full(V) then
         raise Index_Out_Of_Bounds_Error;
      end if;
      V.Size := V.Size + 1;
      V.Items(V.Size) := Item;
   end Add;


   overriding procedure Add (V     : in out Vector_Type;
                             Index : in     Index_Type;
                             Item  : in     Item_Type)
   is begin
      if Is_Full(V) then
         raise Index_Out_Of_Bounds_Error;
      end if;
      Shift(V.Items, Index, V.Size, +1);
      V.Size := V.Size + 1;
   end Add;


   overriding procedure Remove (V : in out Vector_Type; Item : in Item_Type)
   is begin
      for I in 1 .. V.Size loop
         if V.Items(I) = Item then
            Shift(V.Items, Index + 1, V.Size, -1);
            V.Size := V.Size - 1;
            return;
         end if;
      end loop;
   end Remove;


   overriding procedure Remove (V : in out Vector_Type; Index : in Index_Type)
   is begin
      if Index not in 1 .. V.Size then
         raise No_Such_Element_Exception;
      end if;
      Shift(V.Items, Index + 1, V.Size, -1);
      V.Size := V.Size - 1;
   end Remove;


   overriding function Index_Of (V : Vector_Type; Item : Item_Type)
      return Index_Type
   is begin
      for I in 1 .. V.Size loop
         if V.Items(I) = Item then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Of;


   overriding function Item (V : Vector_Type; Index : Index_Type)
      return Item_Type
   is begin
      if Index not in 1 .. V.Size then
         raise No_Such_Element_Exception;
      end if;
      return V.Items(Index);
   end if;


   overriding function Contains (V : Vector_Type; Item : Item_Type)
      return Boolean
   is begin
      return Index_Of(V, Item) /= 0;
   end Contains;


   overriding function Is_Empty (V : Vector_Type) return Boolean
   is begin
      return V.Size = 0;
   end Is_Empty;


   overriding function Is_Full (V : Vector_Type) return Boolean
   is begin
      return V.Size = V.Capacity;
   end Is_Full;


   overriding function Size (V : Vector_Type) return Size_Type
   is begin
      return V.Size;
   end Size;

end Gen_Containers.Gen_Vectors;

