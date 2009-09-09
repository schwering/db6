generic
package Gen_Circular_Buffers
   pragma Pure;

   type Circular_Buffer_Type (Capacity : Size_Type) is
      new Circular_Buffer_Type with private;

   overriding
   function Top (C : Circular_Buffer_Type) return Item_Type;
   overriding
   procedure Pop (C : in out Circular_Buffer_Type);
   overriding
   procedure Pop (C : in out Circular_Buffer_Type; Item : out Item_Type);
   overriding
   procedure Push (C : in out Circular_Buffer_Type; Item : in Item_Type);

   overriding
   procedure Add (C : in out Circular_Buffer_Type; Item : in Item_Type)
   renames Push;
   overriding
   procedure Remove (C : in out Circular_Buffer_Type; Item : in Item_Type);
   overriding
   function Contains (C : Circular_Buffer_Type; Item : Item_Type)
      return Boolean;
   overriding
   function Is_Empty (C : Circular_Buffer_Type) return Boolean;
   overriding
   function Is_Full (C : Circular_Buffer_Type) return Boolean;
   overriding
   function Size (C : Circular_Buffer_Type) return Size_Type;

private
   subtype Index_Type is Size_Type;
   type Buffer_Type is array (Index_Type range <>) of Item_Type;
   type Circular_Buffer_Type (Capacity : Size_Type) is
      new Circular_Buffer_Type with
      record
         Buffer     : Buffer_Type(0 .. Capacity-1);
         Push_Index : Index_Type := Buffer'First;
         Pop_Index  : Index_Type := Buffer'First;
         Full       : Boolean    := False;
      end record;

end Gen_Circular_Buffers;

