generic
   type Key_Type is private;
   type Value_Type is private;
   with function Copy (Item : Item_Type) return Item_Type;
   with procedure Finalize (Item : in out Item_Type);
package Gen_Maps is

   type Size_Type is new Natural;
   subtype Index_Type is Size_Type;

   No_Such_Element_Error       : exception;
   Unsupported_Operation_Error : exception;

   type Map_Type is limited interface;
   procedure Put (M     : in out Map_Type;
                  Key   : in     Key_Type;
                  Value : in     Value_Type)
   is abstract;
   function Get (M : Map_Type; Key : Key_Type) return Value_Type
   is abstract;
   procedure Delete (M : in out Map_Type; Key : in Key_Type)
   is abstract; 
   procedure Delete (M     : in out Map_Type;
                     Key   : in     Key_Type;
                     Value :    out Value_Type)
   is abstract;
   function Size (M : Map_Type) return Size_Type
   is abstract;
   type Synchronized_Map_Type is synchronized interface and Map_Type;

end Gen_Maps;

