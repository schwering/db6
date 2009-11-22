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
-- Copyright 2008, 2009 Christoph Schwering

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

end DB.Tables;

