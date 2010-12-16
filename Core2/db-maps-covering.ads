-- Abstract:
--
-- Maps implementation using normal Gen_BTrees with Types.Keys and
-- Types.Values.Covering.
--
-- Copyright 2008, 2009, 2010 Christoph Schwering

with DB.Utils.Gen_Binary_Heaps;
with DB.Utils.Regular_Expressions;

package DB.Maps.Covering is

   ----------
   -- Map initialization operations.

   type Map_Type is new Maps.Map_Type with private;

   not overriding
   function New_Map
     (Allow_Duplicates : in Boolean := Default_Allow_Duplicates)
      return Map_Type;

   not overriding
   procedure Add_Slice
     (Map   : in out Map_Type;
      Guard : in     String;
      Impl  : in     Implementation_Type;
      ID    : in     String);

   overriding
   procedure Create (Map : in out Map_Type; ID : in String);
   -- Creates a new map named ID or raises a DB.IO_Error when creation
   -- fails.

   overriding
   procedure Create_Temporary (Map : in out Map_Type; ID : in String);
   -- Creates a new temporary map named ID or raises a DB.IO_Error when creation
   -- fails.

   overriding
   procedure Open (Map : in out Map_Type; ID : in String);
   -- Opens Map with the map named ID.

   overriding
   procedure Finalize (Map : in out Map_Type);
   -- Finalizes Map, i.e. closes opened files.

   overriding
   function Max_Key_Size
     (Map            : Map_Type;
      Max_Value_Size : Blocks.Size_Type)
      return Blocks.Size_Type;
   -- Returns the maximum allowed size of keys.


   ----------
   -- Core operations: Search, Insertion, Deletion.

   overriding
   function Contains
     (Map : Map_Type;
      Key : Key_Type)
      return Boolean;

   overriding
   procedure Search
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type);

   overriding
   procedure Search_Minimum
     (Map   : in out Map_Type;
      Key   :    out Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type);

   overriding
   procedure Insert
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value : in     Value_Type'Class;
      State :    out State_Type);

   overriding
   procedure Insert
     (Map              : in out Map_Type;
      Key              : in     Key_Type;
      Value            : in     Value_Type'Class;
      Allow_Duplicates : in     Boolean;
      State            :    out State_Type);

   overriding
   procedure Delete
     (Map   : in out Map_Type;
      Key   : in     Key_Type;
      Value :    out Value_Type'Class;
      State :    out State_Type);


   ----------
   -- Miscellaneous procedures.

   overriding
   procedure Count
     (Map   : in out Map_Type;
      Count :    out Count_Type);

   overriding
   procedure Reorganize
     (Map   : in out Map_Type;
      State :    out State_Type);

   overriding
   procedure Check
     (Map : in out Map_Type);

   overriding
   procedure Stats
     (Map  : in out Map_Type;
      Emit : not null access procedure (Level : in Level_Type;
                                        Key   : in String;
                                        Value : in Data_Type));


   ----------
   -- Cursor operations.

   type Cursor_Type is new Maps.Cursor_Type with private;

   overriding
   function New_Cursor
     (Map         : Map_Type;
      Thread_Safe : Boolean;
      Lower_Bound : Bound_Type;
      Upper_Bound : Bound_Type)
      return Maps.Cursor_Type'Class;

   overriding
   procedure Finalize
     (Cursor : in out Cursor_Type);

   overriding
   procedure Set_Thread_Safety
     (Cursor  : in out Cursor_Type;
      Enabled : in     Boolean);

   overriding
   procedure Pause
     (Cursor : in out Cursor_Type);

   overriding
   procedure Next
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type);

   overriding
   procedure Delete
     (Cursor : in out Cursor_Type;
      Key    :    out Key_Type;
      Value  :    out Value_Type'Class;
      State  :    out State_Type);

private
   type Map_Ref_Type is access all Map_Type;
   pragma Controlled (Map_Ref_Type);
   for Map_Ref_Type'Storage_Size use 0;

   package RE renames Utils.Regular_Expressions;

   type Node_Type;
   type Node_Ref_Type is access Node_Type;
   type Node_Type (Guard_Length, Impl_Length, ID_Length : Natural) is
      record
         Guard : String (1 .. Guard_Length);
         Impl  : String (1 .. Impl_Length);
         ID    : String (1 .. ID_Length);
         Next  : Node_Ref_Type;
      end record;

   subtype Base_Map_Type is Maps.Map_Type'Class;
   type Base_Map_Ref_Type is access Base_Map_Type;

   type Slice_Type is
      record
         Guard : RE.Regexp_Type;
         Map   : Base_Map_Ref_Type;
      end record;

   type Slice_Array_Type is array (Positive range <>) of Slice_Type;
   type Slice_Array_Ref_Type is access Slice_Array_Type;

   type Cover_Index_Type is new Natural;
   type Cover_Type is array (Cover_Index_Type range <>) of Positive;
   -- Cover_Index_Type is only used as Cover_Type's index to avoid stupid errors
   -- like:
   --    for I in Cover'Range loop ... Slices (       I ) ... end loop;
   -- instead of
   --    for I in Cover'Range loop ... Slices (Cover (I)) ... end loop;
   type Cover_Ref_Type is access Cover_Type;

   function Cover
     (R      : RE.Regexp_Type;
      Slices : Slice_Array_Type)
      return Cover_Type;

   type Map_Type is new Maps.Map_Type with
      record
         Initialized      : Boolean;
         Allow_Duplicates : Boolean;
         Config           : Node_Ref_Type        := null;
         Slices           : Slice_Array_Ref_Type := null;
         Cover            : Cover_Ref_Type       := null;
         Self             : Map_Ref_Type         := Map_Type'Unchecked_Access;
      end record;


   subtype Base_Cursor_Type is Maps.Cursor_Type'Class;
   type Base_Cursor_Ref_Type is access Base_Cursor_Type;
   type Base_Cursor_Ref_Array_Type is array (Positive range <>) of
      Base_Cursor_Ref_Type;
   type Base_Cursor_Ref_Array_Ref_Type is access Base_Cursor_Ref_Array_Type;

   type Value_Ref_Type is access Value_Type'Class;

   type Heap_Item_Type is
      record
         Key    : Key_Type;
         Value  : Value_Ref_Type;
         Cursor : Base_Cursor_Ref_Type;
      end record;

   function "<" (Left, Right : Heap_Item_Type) return Boolean;
   package Heaps is new Utils.Gen_Binary_Heaps (Heap_Item_Type, "<");
   type Heap_Ref_Type is access Heaps.Heap_Type;

   type Cursor_Type is new Maps.Cursor_Type with
      record
         Initialized : Boolean;
         Map         : Map_Ref_Type;
         Cursors     : Base_Cursor_Ref_Array_Ref_Type := null;
         Heap        : Heap_Ref_Type                  := null;
         Heap_Filled : Boolean;
      end record;

end DB.Maps.Covering;

