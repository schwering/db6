package DB.Utils.Binary_Search is
   pragma Pure;

   generic
      type Index_Type is range <>;
      type Item_Type is limited private;
      type Array_Type is array (Index_Type range <>) of Item_Type;
      with function "<=" (A, B : Item_Type) return Boolean is <>;
      with function "=" (A, B : Item_Type) return Boolean is <>;
   procedure Find_Exact
     (Arr   : in  Array_Type;
      Item  : in  Item_Type;
      Found : out Boolean;
      Index : out Index_Type);

   generic
      type Index_Type is range <>;
      type Item_Type is limited private;
      type Array_Type is array (Index_Type range <>) of Item_Type;
      with function "<=" (A, B : Item_Type) return Boolean is <>;
   procedure Find_Best
     (Arr   : in  Array_Type;
      Item  : in  Item_Type;
      Found : out Boolean;
      Index : out Index_Type);

   generic
      type Container_Type is limited private;
      type Index_Type is range <>;
      type Item_Type is limited private;
      with function Get (C : Container_Type; I : Index_Type) return Item_Type;
      with function "<=" (A, B : Item_Type) return Boolean is <>;
      First_Index : in Index_Type;
      Last_Index  : in Index_Type;
   procedure Find_Best_In_Container
     (Container : in  Container_Type;
      Item      : in  Item_Type;
      Found     : out Boolean;
      Index     : out Index_Type);

private
   pragma Inline (Find_Exact);
   pragma Inline (Find_Best);
   pragma Inline (Find_Best_In_Container);

end DB.Utils.Binary_Search;

