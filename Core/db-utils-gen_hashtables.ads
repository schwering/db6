-- Abstract:
--
-- A closed non-growing hashtable implementation.
--
-- This hashtable uses handles collisions by rehashing.
-- The table has a fixed size and does not grow.
--
-- References:
--
-- [Ney] H. Ney -- Datenstrukturen und Algorithmen, Vorlesungsskript
-- http://www-i6.informatik.rwth-aachen.de/web/Teaching/Lectures/SS04/ \
-- Datenstrukturen/index.html#skript
--
-- Design Notes:
--
-- The current implementation is not very good, see the comment at the formal
-- generic parameter Rehash.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with System.Storage_Pools;

generic
   Visit_Threshold : in Float := 0.15;

   type Key_Type is private;
   type Value_Type is private;

   with function Hash (K : Key_Type) return Hash_Type;
   with function Rehash (H : Hash_Type) return Hash_Type;
   -- Note: Currently, the hashtable is implemented that badly that Rehash must
   -- simply add 1, otherwise you might end up in an infinite loop somewhere.
   with function "=" (K, L : Key_Type) return Boolean is <>;

   Storage_Pool : in out System.Storage_Pools.Root_Storage_Pool'Class;
package DB.Utils.Gen_Hashtables is
   pragma Preelaborate;

   type Table_Type (<>) is private;
   type Table_Ref_Type is access Table_Type;
   for Table_Ref_Type'Storage_Pool use Storage_Pool;

   function New_Table
     (Size : Size_Type)
      return Table_Type;

   procedure Allocate_Table
     (Size  : in  Size_Type;
      Table : out Table_Ref_Type);

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
      return Size_Type;

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
   type Array_Type is array (Hash_Type range <>) of Element_Type;
   type Table_Type (Last_Index : Hash_Type) is
      record
         Arr    : Array_Type(0 .. Last_Index)
                := (others => Element_Type'(State => Free));
         Size   : Size_Type := 0;
         Visits : Size_Type := 0;
      end record;

   pragma Inline (Size);

end DB.Utils.Gen_Hashtables;

