generic
package Gen_Containers.Gen_Vectors is

   type Vector_Type (Capacity : Size_Type) is new List_Type with private;

   overriding procedure Add (V : in out Vector_Type; Item : in Item_Type);
   overriding procedure Add (V     : in out Vector_Type;
                             Index : in     Index_Type;
                             Item  : in     Item_Type);
   overriding procedure Remove (V : in out Vector_Type; Item : in Item_Type);
   overriding procedure Remove (V : in out Vector_Type; Index : in Index_Type);
   overriding function Index_Of (V : Vector_Type; Item : Item_Type)
      return Index_Type;
   overriding function Item (V : Vector_Type; Index : Index_Type)
      return Item_Type;
   overriding function Contains (V : Vector_Type; Item : Item_Type)
      return Boolean;
   overriding function Is_Empty (V : Vector_Type) return Boolean;
   overriding function Is_Full (V : Vector_Type) return Boolean;
   overriding function Size (V : Vector_Type) return Size_Type;

private
   type Item_Array_Type is array (Size_Type range <>) of Item_Type;

   type Vector_Type (Capacity : Size_Type) is new Collection_Type with
      record
         Size  : Index_Type := 0;
         Items : Item_Array_Type(1 .. Capacity);
      end record;

end Gen_Containers.Gen_Vectors;

