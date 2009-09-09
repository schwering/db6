generic
   type Item_Type is private;
   with function "=" (A, B : Item_Type) return Boolean is <>;
package Gen_Containers is
   pragma Pure;

   type Size_Type is new Natural;
   subtype Index_Type is Size_Type;

   No_Such_Element_Error       : exception;
   Unsupported_Operation_Error : exception;
   Index_Out_Of_Bounds_Error   : exception;


   -- Collections are the base type for the class of containers that contain
   -- single items.
   -- For a container, there is no ordering or so of the objects. So it is just
   -- that Add + Remove = identity, nothing more can be said. The other
   -- operations are self-explanatory.
   --
   -- Implementations: There are no specific Collection-implementations, so
   -- check the subinterfaces.
   type Collection_Type is limited interface;
   procedure Add (C : in out Collection_Type; Item : in Item_Type)
   is abstract;
   procedure Remove (C : in out Collection_Type; Item : in Item_Type)
   is abstract;
   function Contains (C : Collection_Type; Item : Item_Type) return Boolean
   is abstract;
   function Is_Empty (C : Collection_Type) return Boolean
   is abstract;
   function Is_Full (C : Collection_Type) return Boolean
   is abstract;
   function Size (C : Collection_Type) return Size_Type
   is abstract;
   type Synchronized_Collection_Type is synchronized interface and
      Collection_Type;


   -- Lists are collections with access-by-index.
   -- If an item is inserted at Index, calls to Item with this Index must
   -- return this item. Index_Of might however return another index if the
   -- item is contained multiple times in the list.
   --
   -- Implementations: Vector (with fixed size), Dynamic_Vector (growing on
   -- heap), both with an array in the background, Linked_List and 
   -- Double_Linked_List (both growing on heap, of course). Additionally, the
   -- implementations of Stack.
   type List_Type is limited interface and Collection_Type;
   procedure Add (L     : in out List_Type;
                  Index : in     Index_Type;
                  Item  : in     Item_Type)
   is abstract;
   procedure Remove (L     : in out List_Type;
                     Index : in     Index_Type)
   is abstract;
   function Index_Of (L    : List_Type;
                      Item : Item_Type)
                      return Index_Type
   is abstract;
   function Item (L     : List_Type;
                  Index : Index_Type)
                  return Item_Type
   is abstract;
   type Synchronized_List_Type is synchronized interface and List_Type and
      Synchronized_Collection_Type;


   -- Stacks are well known.
   --
   -- Implementations: Any List implementation could be used as stack
   -- (formal generic package).
   type Stack_Type is limited interface and List_Type;
   function Top (S : Stack_Type) return Item_Type
   is abstract;
   procedure Pop (S : in out Stack_Type)
   is abstract;
   procedure Pop (S : in out Stack_Type; Item : out Item_Type)
   is abstract;
   procedure Push (S : in out Stack_Type; Item : in Item_Type)
   is abstract;
   type Synchronized_Stack_Type is synchronized interface and Stack_Type and
      Synchronized_List_Type;


   -- Queues are well known.
   --
   -- Implementations: Circular_Buffer (with fixed size), Linked_Queue
   -- (growing on heap).
   type Queue_Type is limited interface and Collection_Type;
   function Top (Q : Queue_Type) return Item_Type
   is abstract;
   procedure Pop (Q : in out Queue_Type)
   is abstract;
   procedure Pop (Q : in out Queue_Type; Item : out Item_Type)
   is abstract;
   procedure Push (Q : in out Queue_Type; Item : in Item_Type)
   is abstract;
   type Synchronized_Queue_Type is synchronized interface and Queue_Type and
      Synchronized_Collection_Type;


   -- Sets can contain each element at most once. That's the only difference
   -- to Collections.
   --
   -- Implementations: Hash_Set (with fixed size), Dynamic_Hash_Set (growing
   -- on heap), both with an array and a hash function. Additionally, the
   -- implementations of Sorted_Set.
   type Set_Type is limited interface and Collection_Type;
   type Synchronized_Set_Type is synchronized interface and Set_Type and
      Synchronized_Collection_Type;


   -- Sets can contain each element at most once. That's the only difference
   -- to Collections.
   --
   -- Implementations: Binary_Heap (with fixed size), Binary_Tree (growing
   -- on heap).
   type Sorted_Set_Type is limited interface and Set_Type and List_Type;
   type Synchronized_Sorted_Set_Type is synchronized interface and
      Sorted_Set_Type and Synchronized_Set_Type and Synchronized_List_Type;

end Gen_Containers;

