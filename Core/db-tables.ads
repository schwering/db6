-- Abstract:
--
-- Root package of tables.
--
-- Design Note:
--
-- There are keys and values.
--
-- The keys are defined DB.Types.Keys, so there's nothing special about them.
-- That's perfectly okay because the key type is always the same -- consisting
-- of a string for the row ID, one for the attribute and a timestamp.
--
-- The values however are not homogeneous. The problem is to map the dynamically
-- different value types to a single value type which is needed as formal
-- generic parameter for the BTrees and Blob_Trees packages.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Types.Keys;
with DB.Types.Values.Bounded;
with DB.Types.Values.Unbounded;

package DB.Tables is

   ----------
   -- Key type is predefined.

   subtype Key_Type is Types.Keys.Key_Type;

   ----------
   -- Value type to be implemented by users.

   type Value_Type is interface;

   function "=" (Left, Right : Value_Type) return Boolean
   is abstract;
   -- In fact, this one is needed nowhere. It seems to me that if for a derived
   -- type X "=" is not overridden, then all comparisons of objects of type X
   -- yield `unequal' even when this is not expected without any compiler
   -- warning. Declaring "=" as abstract here avoids in advance errors that
   -- are annyoing to find.

   function To_Bounded
     (Value : Value_Type)
      return Types.Values.Bounded.String_Type
   is abstract;

   function From_Bounded
     (Buffer : Types.Values.Bounded.String_Type)
      return Value_Type
   is abstract;

   function To_Unbounded
     (Value : Value_Type)
      return Types.Values.Unbounded.String_Type
   is abstract;

   function From_Unbounded
     (Buffer : Types.Values.Unbounded.String_Type)
      return Value_Type
   is abstract;

   function Image
     (Value : Value_Type)
      return String
   is abstract;


--   type Table_Type is private;

--with DB.Tables.Column_Families;
--with DB.Tables.Maps;
--with DB.Utils.Gen_Linked_Lists;
--with DB.Utils.Gen_Smart_Pointers;
--with DB.Utils.Global_Pool;
--private
   --subtype CF_Type is Column_Families.Column_Family_Type;
   --type CF_Ref_Type is access CF_Type;

   --package CF_Pointers is new Utils.Gen_Smart_Pointers
     --(CF_Type, CF_Ref_Type);

   --function "=" (A, B : CF_Pointers.Smart_Pointer_Type) return Boolean;

   --package CF_Lists is new Utils.Gen_Linked_List
     --(CF_Pointers.Smart_Pointer_Type, "=",
      --Utils.Global_Pool.Global'Storage_Pool);

   --type Table_Type is
      --record
         --Families : CF_Lists.List_Type;
      --end record;

   --function "=" (A, B : CF_Pointers.Smart_Pointer_Type) return Boolean is
   --begin
      --return CF_Pointers.Ref(A).Guard = CF_Pointers.Ref(B).Guard;
   --end "=";


end DB.Tables;

