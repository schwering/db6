generic
   Visit_Threshold : in Float := 0.15;
   Size_Threshold  : in Float := 0.65;
   type Hash_Type is (<>);
   type Key_Type is private;
   type Value_Type is private;
   with function Hash (K : Key_Type) return Hash_Type;
   with function Rehash (H : Hash_Type) return Hash_Type;
   with function "=" (K, L : Key_Type) return Boolean is <>;
package DB.Utils.Gen_Hashtables is
   pragma Pure;

   type Table_Type is private;

   procedure Put
     (Table : in out Table_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type);

   procedure Delete
     (Table : in out Table_Type;
      Key   : in     Key_Type);

   procedure Get
     (Table : in  Table_Type;
      Key   : in  Key_Type;
      Value : out Value_Type;
      Found : out Boolean);

   function Get
     (Table : Table_Type;
      Key   : Key_Type)
      return Value_Type;

   function Contains
     (Table : Table_Type;
      Key   : Key_Type)
      return Boolean;

   procedure Pop
     (Table : in out Table_Type;
      Key   :    out Key_Type;
      Value :    out Value_Type);

   procedure Pop
     (Table   : in out Table_Type;
      Key     :    out Key_Type;
      Value   :    out Value_Type;
      Success :    out Boolean);

   function Size
     (Table : Table_Type)
      return Natural;

   Hash_Table_Error : exception;

private
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
   type Array_Type is array (Hash_Type) of Element_Type;
   type Table_Type is
      record
         Arr    : Array_Type := (others => Element_Type'(State => Free));
         Size   : Natural    := 0;
         Visits : Natural    := 0;
      end record;

   pragma Inline (Put);
   pragma Inline (Delete);
   pragma Inline (Get);
   pragma Inline (Pop);
   pragma Inline (Contains);
   pragma Inline (Size);

end DB.Utils.Gen_Hashtables;

